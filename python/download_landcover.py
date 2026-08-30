# ============================================================
# download_landcover.py — download CORINE Land Cover (CLC) raster
#                          snapshots (2006, 2012, 2018) via the
#                          CLMS Download API
#
# Unlike the DEM (public S3) and land use (public Zenodo API)
# downloads, CORINE Land Cover full-Europe rasters are NOT available
# from a stable, unauthenticated, direct-download URL. The Copernicus
# Land Monitoring Service (CLMS) explicitly requires going through
# its Download API, which needs a personal Bearer token. See:
#   https://eea.github.io/clms-api-docs/authentication.html
#   https://eea.github.io/clms-api-docs/download.html
#
# ONE-TIME SETUP (must be done by you -- this cannot be automated,
# account creation is off-limits for an AI assistant to do on your
# behalf):
#   1. Create a free account at https://land.copernicus.eu (EU Login).
#   2. Log in, click your username (top green bar) -> "API Tokens".
#   3. Click "Create new Token", give it a name.
#   4. The page will show client_id, private_key, user_id, token_uri
#      EXACTLY ONCE. Save them as a JSON file, e.g.:
#      {
#        "client_id": "...",
#        "private_key": "-----BEGIN PRIVATE KEY-----\n...\n-----END PRIVATE KEY-----\n",
#        "user_id": "...",
#        "token_uri": "https://land.copernicus.eu/@@oauth2-token"
#      }
#   5. Point the CLMS_TOKEN_JSON environment variable at that file's
#      path (never commit this file -- keep it out of version control).
#
# From there, this script is fully automated: it signs a short-lived
# JWT with your private key, exchanges it for a 1-hour Bearer access
# token, submits one download request per CORINE year, polls until
# each is ready, and downloads + extracts the resulting GeoTIFF.
#
# IMPORTANT -- BoundingBox order discrepancy in the CLMS API docs:
# The prose on the download docs page states the order is
# [max.lat, max.lon, min.lat, min.lon] i.e. [N, E, S, W]. However,
# the docs' OWN worked example uses coordinates that only make
# geographic sense (a small region in France) if the actual order is
# [W, N, E, S] -- i.e. [min.lon, max.lat, max.lon, min.lat]. We use
# the empirically-correct [W, N, E, S] order below (matching the
# worked example's real coordinates), NOT the prose description.
# If CORINE downloads come back with an unexpected extent, this is
# the first thing to re-check -- consider submitting one small test
# request first and inspecting the resulting raster's extent before
# trusting a full-Europe request.
#
# Required environment variables:
#   CLMS_TOKEN_JSON   — path to the API token JSON file (see above)
#   LANDCOVER_OUT_DIR — output directory for downloaded GeoTIFFs
#   LANDCOVER_BBOX_N, LANDCOVER_BBOX_W, LANDCOVER_BBOX_S, LANDCOVER_BBOX_E
#                     — bounding box in WGS84 (EPSG:4326)
#
# Requires: pip install requests pyjwt cryptography
# ============================================================
import json
import os
import time
import zipfile
import io

import jwt
import requests

TOKEN_EXCHANGE_URL_DEFAULT = "https://land.copernicus.eu/@@oauth2-token"
API_BASE = "https://land.copernicus.eu/api"

# CORINE Land Cover raster datasets, resolved once via the CLMS
# @search endpoint (https://eea.github.io/clms-api-docs/download.html)
# and hardcoded here since these UIDs are stable dataset identifiers
# (same pattern as the hardcoded Zenodo record IDs in
# download_landuse.py).
CORINE_DATASETS = {
    2006: {
        "DatasetID": "d443c86fec2f49e08ff12c7decdbf2af",
        "DatasetDownloadInformationID": "4841f8cb-6e7c-4227-bb35-cd787609c546",
        "OutFilename": "U2012_CLC2006_V2020_20u1.tif",
    },
    2012: {
        "DatasetID": "a5ee71470be04d66bcff498f94ceb5dc",
        "DatasetDownloadInformationID": "b1027c88-6256-435b-98a3-62f019cbb381",
        "OutFilename": "U2018_CLC2012_V2020_20u1_raster100m.tif",
    },
    2018: {
        "DatasetID": "0407d497d3c44bcd93ce8fd5bf78596a",
        "DatasetDownloadInformationID": "7bcdf9d1-6ba0-4d4e-afa8-01451c7316cb",
        "OutFilename": "U2018_CLC2018_V2020_20u1.tif",
    },
}


def _sign_and_exchange(service_key):
    """Sign a JWT with the saved private key and exchange it for a
    1-hour Bearer access token. Returns (access_token, expires_at_epoch)."""
    private_key = service_key["private_key"].encode("utf-8")
    now = int(time.time())
    claim_set = {
        "iss": service_key["client_id"],
        "sub": service_key["user_id"],
        "aud": service_key.get("token_uri", TOKEN_EXCHANGE_URL_DEFAULT),
        "iat": now,
        "exp": now + 3600,
    }
    grant = jwt.encode(claim_set, private_key, algorithm="RS256")

    resp = requests.post(
        service_key.get("token_uri", TOKEN_EXCHANGE_URL_DEFAULT),
        data={
            "grant_type": "urn:ietf:params:oauth:grant-type:jwt-bearer",
            "assertion": grant,
        },
    )
    resp.raise_for_status()
    body = resp.json()
    # Refresh a bit early (60s margin) rather than cutting it exactly at expiry.
    expires_at = now + int(body.get("expires_in", 3600)) - 60
    return body["access_token"], expires_at


class TokenManager:
    """Holds the current Bearer token and transparently re-signs/exchanges
    a fresh one when it's about to expire -- needed because a single CLMS
    download task can take longer than the token's 1-hour lifetime."""

    def __init__(self, token_json_path):
        with open(token_json_path) as f:
            self.service_key = json.load(f)
        self.access_token = None
        self.expires_at = 0

    def get(self):
        if time.time() >= self.expires_at:
            self.access_token, self.expires_at = _sign_and_exchange(self.service_key)
        return self.access_token

    def auth_header(self):
        return {"Authorization": f"Bearer {self.get()}"}


def submit_request(token_mgr, dataset_id, download_info_id, bbox_wnes):
    """Submit one CLMS download request restricted to a bounding box.

    bbox_wnes: (W, N, E, S) in EPSG:4326 -- see the module-level note
    above about the CLMS docs' bounding-box order discrepancy.
    """
    w, n, e, s = bbox_wnes
    payload = {
        "Datasets": [{
            "DatasetID": dataset_id,
            "DatasetDownloadInformationID": download_info_id,
            "BoundingBox": [w, n, e, s],
            "OutputFormat": "Geotiff",
            "OutputGCS": "EPSG:3035",
        }]
    }
    resp = requests.post(
        f"{API_BASE}/@datarequest_post",
        headers=token_mgr.auth_header(),
        json=payload,
    )
    resp.raise_for_status()
    body = resp.json()
    if body.get("ErrorTaskIds"):
        raise RuntimeError(f"CLMS rejected the download request: {body}")
    return str(body["TaskIds"][0]["TaskID"])


def poll_task(token_mgr, task_id, timeout_s=7200, interval_s=30):
    """Poll @datarequest_search until `task_id` shows up as finished.
    The token is refreshed automatically (via token_mgr) if it expires
    while waiting. Returns the DownloadURL (valid for 72 hours per the
    CLMS docs)."""
    deadline = time.time() + timeout_s
    while time.time() < deadline:
        resp = requests.get(
            f"{API_BASE}/@datarequest_search",
            params={"status": "Finished_ok"},
            headers=token_mgr.auth_header(),
            timeout=30,
        )
        resp.raise_for_status()
        finished = resp.json()
        if task_id in finished:
            return finished[task_id]["DownloadURL"]

        resp_err = requests.get(
            f"{API_BASE}/@datarequest_search",
            params={"status": "Rejected"},
            headers=token_mgr.auth_header(),
            timeout=30,
        )
        if resp_err.ok and task_id in resp_err.json():
            raise RuntimeError(f"CLMS task {task_id} was rejected: {resp_err.json()[task_id]}")

        print(f"  Task {task_id} not finished yet, waiting {interval_s}s...")
        time.sleep(interval_s)

    raise TimeoutError(f"CLMS task {task_id} did not finish within {timeout_s}s")


def download_and_extract(download_url, out_filename, out_dir):
    """Download the result ZIP and extract the single GeoTIFF inside it
    as `out_filename` under `out_dir`."""
    target_path = os.path.join(out_dir, out_filename)
    print(f"  Downloading result archive from {download_url}...")
    resp = requests.get(download_url, stream=True, timeout=60)
    resp.raise_for_status()

    with zipfile.ZipFile(io.BytesIO(resp.content)) as zf:
        tif_members = [m for m in zf.namelist() if m.lower().endswith(".tif")]
        if len(tif_members) == 0:
            raise RuntimeError(f"No .tif found in downloaded archive for {out_filename}")
        # If more than one, take the largest -- the raster itself
        # rather than any auxiliary/overview file.
        member = max(tif_members, key=lambda m: zf.getinfo(m).file_size)
        with zf.open(member) as src, open(target_path, "wb") as dst:
            dst.write(src.read())

    print(f"  Saved: {target_path}")
    return target_path


if __name__ == "__main__":
    token_json_path = os.environ["CLMS_TOKEN_JSON"]
    out_dir = os.environ["LANDCOVER_OUT_DIR"]
    os.makedirs(out_dir, exist_ok=True)

    bbox_wnes = (
        float(os.environ["LANDCOVER_BBOX_W"]),
        float(os.environ["LANDCOVER_BBOX_N"]),
        float(os.environ["LANDCOVER_BBOX_E"]),
        float(os.environ["LANDCOVER_BBOX_S"]),
    )

    token_mgr = TokenManager(token_json_path)

    for year, info in CORINE_DATASETS.items():
        target_path = os.path.join(out_dir, info["OutFilename"])
        if os.path.exists(target_path) and os.path.getsize(target_path) > 0:
            print(f"CORINE {year}: already exists, skipping: {info['OutFilename']}")
            continue

        print(f"CORINE {year}: submitting download request...")
        task_id = submit_request(token_mgr, info["DatasetID"],
                                  info["DatasetDownloadInformationID"], bbox_wnes)
        print(f"CORINE {year}: task {task_id} submitted, polling for completion...")

        download_url = poll_task(token_mgr, task_id)
        download_and_extract(download_url, info["OutFilename"], out_dir)

    print("\nAll done.")
