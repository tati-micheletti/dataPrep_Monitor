# ============================================================
# download_landuse.py — download crop type maps from Zenodo
#
# Sources:
#   1. Schwieder et al. 2024 = 2017-2021 (record 10640528)
#   2. Tetteh et al. 2025a = 2024        (record 17197830)
#   3. Tetteh et al. 2025b = 2025        (record 17197871)
#   4. Tetteh et al. 2026 = 1990-2023    (record 20815677)
#      NOTE: Tetteh et al. 2026 is distributed as a single zip
#      file (HCTM_Data.zip, 14.7GB) which cannot be selectively
#      extracted via the Zenodo API. This dataset must be
#      downloaded manually from:
#      https://doi.org/10.5281/zenodo.20815677
#      Only the LU_Maps/ subfolder contents are needed.
#      Place the extracted files in the directory given by
#      LANDUSE_OUT_DIR (see below) before running computeLanduse().
#
# IMPORTANT: Tetteh et al. 2026 uses different LU categories than
# Schwieder et al. 2024 — category mapping in getLanduseCategories.R
# handles both datasets.
#
# Adapted for the dataPrep_Monitor SpaDES module: the output
# directory is passed in via an environment variable (set from R
# before this script is run with reticulate::py_run_file()) instead
# of reading config.yaml directly.
#
# Required environment variables:
#   LANDUSE_OUT_DIR — output directory for downloaded files
#
# Requires: pip install requests
# ============================================================
import requests
import os

LANDUSE_DIR = os.environ["LANDUSE_OUT_DIR"]
os.makedirs(LANDUSE_DIR, exist_ok=True)


def download_if_missing(url, target_path, expected_size_bytes=None):
    """Download a file only if it doesn't exist or is incomplete."""
    if os.path.exists(target_path):
        if expected_size_bytes is not None:
            actual_size = os.path.getsize(target_path)
            if actual_size == expected_size_bytes:
                print(f"  Already exists, skipping: "
                      f"{os.path.basename(target_path)}")
                return
            else:
                print(f"  File exists but size mismatch "
                      f"({actual_size} vs {expected_size_bytes} bytes), "
                      f"re-downloading...")
        else:
            print(f"  Already exists, skipping: "
                  f"{os.path.basename(target_path)}")
            return

    print(f"  Downloading {os.path.basename(target_path)}"
          + (f" ({expected_size_bytes / 1e6:.1f} MB)..."
             if expected_size_bytes else "..."))

    with requests.get(url, stream=True) as r:
        r.raise_for_status()
        downloaded = 0
        with open(target_path, "wb") as f:
            for chunk in r.iter_content(chunk_size=8192):
                f.write(chunk)
                downloaded += len(chunk)
                if expected_size_bytes and \
                   downloaded % (100 * 1024 * 1024) < 8192:
                    pct = downloaded / expected_size_bytes * 100
                    print(f"    {pct:.0f}% "
                          f"({downloaded / 1e6:.0f} MB)")

    print(f"  Done: {os.path.basename(target_path)}")


def fetch_zenodo_files(record_id):
    """Fetch file list from Zenodo API for a given record ID."""
    url = f"https://zenodo.org/api/records/{record_id}"
    response = requests.get(url)
    response.raise_for_status()
    return response.json()["files"]


if __name__ == "__main__":
    # Dataset 1: Schwieder et al. 2024 (2017-2021)
    print("Dataset 1: Schwieder et al. 2024 (2017-2021)...")
    files_schwieder = fetch_zenodo_files("10640528")
    files_to_download = [
        f for f in files_schwieder
        if any(f["key"].endswith(ext)
               for ext in (".tif", ".clr", ".pdf"))
    ]
    for file_info in files_to_download:
        target = os.path.join(LANDUSE_DIR, file_info["key"])
        download_if_missing(file_info["links"]["self"],
                             target,
                             file_info["size"])

    # Dataset 2: Tetteh et al. 2025a (2024)
    print("\nDataset 2: Tetteh et al. 2025a (2024)...")
    files_2024 = fetch_zenodo_files("17197830")
    for file_info in files_2024:
        if file_info["key"].endswith(".tif"):
            target = os.path.join(LANDUSE_DIR, file_info["key"])
            download_if_missing(file_info["links"]["self"],
                                 target,
                                 file_info["size"])

    # Dataset 3: Tetteh et al. 2025b (2025)
    print("\nDataset 3: Tetteh et al. 2025b (2025)...")
    files_2025 = fetch_zenodo_files("17197871")
    for file_info in files_2025:
        if file_info["key"].endswith(".tif"):
            target = os.path.join(LANDUSE_DIR, file_info["key"])
            download_if_missing(file_info["links"]["self"],
                                 target,
                                 file_info["size"])

    # Dataset 4: Tetteh et al. 2026 (1990-2023) — manual download, see note above
    print("\nDataset 4: Tetteh et al. 2026 (1990-2023) — manually downloaded.")
    hctm_files = [f for f in os.listdir(LANDUSE_DIR)
                  if f.startswith("HCTM_GER_") and f.endswith(".tif")]
    print(f"  Found {len(hctm_files)} HCTM files in {LANDUSE_DIR}")

    # Summary
    print("\nDownload summary:")
    datasets = {
        "Schwieder et al. 2024 (2017-2021)": [
            f"CTM_GER_{yr}_rst_v202_COG.tif"
            for yr in range(2017, 2022)],
        "Tetteh et al. 2025a (2024)": [
            "CTM_GER_2024_rst_v302_COG.tif"],
        "Tetteh et al. 2025b (2025)": [
            "CTM_GER_2025_rst_v302_2025_08_COG.tif"],
        "Tetteh et al. 2026 (1990-2023, manual)": [
            f"HCTM_GER_{yr}_rst_v101_COG.tif"
            for yr in range(1990, 2024)],
    }

    for dataset, files in datasets.items():
        present = sum(1 for f in files
                      if os.path.exists(os.path.join(LANDUSE_DIR, f)))
        print(f"  {dataset}: {present}/{len(files)} files present")
