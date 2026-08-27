# ============================================================
# download_dem.py — download Copernicus GLO-30 DEM tiles
#
# Source: Copernicus GLO-30 Public DEM on AWS S3
# No authentication needed — open access
# Tiles are 1x1 degree COG GeoTIFFs at 30m resolution
#
# Adapted for the dataPrep_Monitor SpaDES module: parameters are
# passed in via environment variables (set from R before this script
# is run with reticulate::py_run_file()) instead of reading
# config.yaml directly, so the module does not depend on the
# project's config.yaml.
#
# Required environment variables:
#   DEM_OUT_DIR   — output directory for downloaded tiles
#   DEM_BBOX_N, DEM_BBOX_W, DEM_BBOX_S, DEM_BBOX_E — bounding box (WGS84)
#
# Requires: pip install requests
# ============================================================
import requests
import os

DEM_DIR = os.environ["DEM_OUT_DIR"]
os.makedirs(DEM_DIR, exist_ok=True)

# GLO-30 public S3 base URL — no credentials needed
S3_BASE = "https://copernicus-dem-30m.s3.amazonaws.com"

# ── Derive tile grid from bounding box ───────────────────────────────────────
# Tiles are named by their SW corner (floor of lat/lon)
lat_max = int(float(os.environ["DEM_BBOX_N"]))
lon_min = int(float(os.environ["DEM_BBOX_W"]))
lat_min = int(float(os.environ["DEM_BBOX_S"]))
lon_max = int(float(os.environ["DEM_BBOX_E"]))

# Generate all 1x1 degree tile corners covering the bbox
lats = range(lat_min, lat_max + 1)
lons = range(lon_min, lon_max + 1)

print(f"Bounding box: N{lat_max} W{lon_min} S{lat_min} E{lon_max}")
print(f"Potential tiles: {len(list(lats))} lat x {len(list(lons))} lon "
      f"= {len(list(lats)) * len(list(lons))} tiles")
print("(Not all will exist — ocean/missing tiles skipped automatically)\n")


def build_tile_name(lat, lon):
    """
    Build the GLO-30 tile name from integer lat/lon of SW corner.
    Format: Copernicus_DSM_COG_10_N47_00_E005_00_DEM
    Handles negative lat (S) and lon (W) correctly.
    """
    lat_str = f"{'N' if lat >= 0 else 'S'}{abs(lat):02d}_00"
    lon_str = f"{'E' if lon >= 0 else 'W'}{abs(lon):03d}_00"
    return f"Copernicus_DSM_COG_10_{lat_str}_{lon_str}_DEM"


def download_tile(tile_name, out_dir):
    """Download a single GLO-30 tile if it exists and isn't already saved."""
    filename = f"{tile_name}.tif"
    target_path = os.path.join(out_dir, filename)

    # Cache check
    if os.path.exists(target_path) and os.path.getsize(target_path) > 0:
        print(f"  Already exists, skipping: {filename}")
        return "skipped"

    # GLO-30 S3 structure: /TileName/TileName.tif
    url = f"{S3_BASE}/{tile_name}/{filename}"

    # HEAD request first — cheap way to check tile exists before downloading
    head = requests.head(url)
    if head.status_code == 404:
        print(f"  No tile (ocean/gap): {filename}")
        return "missing"
    elif head.status_code != 200:
        print(f"  Unexpected status {head.status_code} for {filename}, skipping.")
        return "error"

    size_bytes = int(head.headers.get("Content-Length", 0))
    print(f"  Downloading {filename} ({size_bytes / 1e6:.1f} MB)...")

    with requests.get(url, stream=True) as r:
        r.raise_for_status()
        downloaded = 0
        with open(target_path, "wb") as f:
            for chunk in r.iter_content(chunk_size=8192):
                f.write(chunk)
                downloaded += len(chunk)
                if size_bytes and downloaded % (50 * 1024 * 1024) < 8192:
                    pct = downloaded / size_bytes * 100
                    print(f"    {pct:.0f}%")

    print(f"  Done: {filename}")
    return "downloaded"


if __name__ == "__main__":
    results = {"downloaded": 0, "skipped": 0, "missing": 0, "error": 0}

    for lat in lats:
        for lon in lons:
            tile_name = build_tile_name(lat, lon)
            status = download_tile(tile_name, DEM_DIR)
            results[status] += 1

    print("\nAll done.")
    print(f"  Downloaded : {results['downloaded']} tiles")
    print(f"  Skipped    : {results['skipped']} tiles (already existed)")
    print(f"  Missing    : {results['missing']} tiles (ocean or data gap)")
    print(f"  Errors     : {results['error']} tiles")
    print(f"\nTiles saved to: {DEM_DIR}")
