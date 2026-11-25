"""Download the Thuenen (ThEO) crop type maps through the STAC API, without file names written into the code (2026-11-25).

  python download_ctm_stac.py --dest <folder> --years 2017-2026 [--collection crop-type-map-latest] [--list-only]

For every year the item `<collection>-<year>` is read from https://eodata.thuenen.de/stac/api/v1 and its `data` asset is downloaded if the file is not in <dest>
or has another size. A map of an unfinished year has the last month in its name (CTM_GER_2026_rst_v302_2026_09_COG.tif); the finished map has the plain name,
so a re-run after a new release downloads the new file next to the old one (nothing is deleted or overwritten; findCtmFile() in dataPrep_Monitor picks the best).
Writes <dest>/ctm_registry.csv (year, file, size, stac_updated, status, downloaded_at), one row per file seen in this call.
"""
import argparse, csv, datetime, json, os, sys, urllib.request

API = "https://eodata.thuenen.de/stac/api/v1"


def get(url):
    with urllib.request.urlopen(urllib.request.Request(url, headers={"User-Agent": "birdMonitor-ctm-fetch"}), timeout=60) as r:
        return json.load(r)


def parse_years(s):
    out = []
    for part in s.split(","):
        a, _, b = part.partition("-")
        out += list(range(int(a), int(b or a) + 1))
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dest", required=True); ap.add_argument("--years", required=True)
    ap.add_argument("--collection", default="crop-type-map-latest"); ap.add_argument("--list-only", action="store_true")
    a = ap.parse_args(); os.makedirs(a.dest, exist_ok=True); rows = []
    for y in parse_years(a.years):
        try:
            item = get(f"{API}/collections/{a.collection}/items/{a.collection}-{y}")
        except Exception as e:
            print(f"{y}: no item in {a.collection} ({e})"); continue
        asset = item["assets"]["data"]; href = asset["href"]; name = href.rsplit("/", 1)[1]; size = int(asset.get("file:size") or 0)
        status = "in-season" if f"_{y}_" in name.replace(f"CTM_GER_{y}_", "", 1) else "final"
        path = os.path.join(a.dest, name); have = os.path.exists(path) and (not size or os.path.getsize(path) == size)
        when = ""
        if have:
            print(f"{y}: {name} already here ({size/1e6:.0f} MB, {status})")
        elif a.list_only:
            print(f"{y}: would download {name} ({size/1e6:.0f} MB, {status}, STAC updated {item['properties'].get('updated','')[:10]})")
        else:
            print(f"{y}: downloading {name} ({size/1e6:.0f} MB, {status}) ...", flush=True)
            part = path + ".part"
            urllib.request.urlretrieve(href, part)
            if size and os.path.getsize(part) != size:
                sys.exit(f"{y}: size of {name} is {os.path.getsize(part)}, expected {size}; left as {part}")
            os.replace(part, path); when = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S"); print(f"{y}: done")
        rows.append([y, name, size, item["properties"].get("updated", ""), status, when])
    if not a.list_only and rows:
        reg = os.path.join(a.dest, "ctm_registry.csv"); new = not os.path.exists(reg)
        with open(reg, "a", newline="") as f:
            w = csv.writer(f)
            if new: w.writerow(["year", "file", "size", "stac_updated", "status", "downloaded_at"])
            w.writerows(rows)


if __name__ == "__main__":
    main()
