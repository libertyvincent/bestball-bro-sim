# Throwaway: verify which Underdog slate UUID is actually superflex,
# directly from the RAW udbb-scraper export (not derived artifacts).
#
# Structural signals per slate UUID:
#   1. draft length: superflex runs 20 rounds (240 picks/draft), standard 18 (216)
#   2. QB-drafting pattern: superflex -> QBs go top-10 overall, teams roster 4-6 QBs
#   3. any slot/lineup metadata captured in the export (SFLEX slot)
import json
import sys
from collections import Counter, defaultdict

PATH = "inst/data/scraped_drafts/udbb-scraper-latest.json"
TARGETS = {
    "0ee05b31-f904-4e6e-985b-42f290e3aa3a": "manifest says: nfl_2026_eliminator",
    "11300882-a76f-4e12-8a5e-1a6b4c513794": "manifest says: nfl_2026_superflex",
}

with open(PATH, encoding="utf-8") as f:
    data = json.load(f)

print("== top-level keys ==")
print(list(data.keys()) if isinstance(data, dict) else type(data))


def walk_find(obj, key, limit=3, found=None):
    """Find sample objects containing a given key anywhere in the tree."""
    if found is None:
        found = []
    if len(found) >= limit:
        return found
    if isinstance(obj, dict):
        if key in obj:
            found.append(obj)
        for v in obj.values():
            walk_find(v, key, limit, found)
    elif isinstance(obj, list):
        for v in obj:
            walk_find(v, key, limit, found)
    return found


# ---------------------------------------------------------------------------
# Locate draft objects: they carry slate_id + picks.
# ---------------------------------------------------------------------------
drafts = walk_find(data, "draft_state", limit=10**9)
print(f"\n== drafts found: {len(drafts)} ==")

# Player catalogs for position lookup: appearance_id -> player -> position.
# (load_scraped_drafts does pick.appearance_id -> appearance.player_id -> player)
appearances = {}
for ap in walk_find(data, "player_id", limit=10**9):
    if "id" in ap and isinstance(ap.get("player_id"), str) and "points" not in ap:
        appearances[ap["id"]] = ap.get("player_id")
players = {}
for pl in walk_find(data, "position_name", limit=10**9):
    if "id" in pl and "first_name" in pl:
        players[pl["id"]] = (
            pl.get("first_name", ""), pl.get("last_name", ""), pl.get("position_name", ""))
print(f"appearance map: {len(appearances)} | player map: {len(players)}")

# ---------------------------------------------------------------------------
# Per-slate draft structure
# ---------------------------------------------------------------------------
by_slate = defaultdict(list)
for d in drafts:
    sid = d.get("slate_id")
    by_slate[sid].append(d)

print("\n== drafts per slate UUID ==")
for sid, ds in sorted(by_slate.items(), key=lambda kv: -len(kv[1])):
    label = TARGETS.get(sid, "")
    print(f"  {sid}  n_drafts={len(ds)}  {label}")

for sid, label in TARGETS.items():
    ds = by_slate.get(sid, [])
    print(f"\n========== slate {sid[:8]}...  ({label}) ==========")
    if not ds:
        print("  NO DRAFTS for this slate UUID in the export")
        continue
    for d in ds:
        picks = d.get("picks") or walk_find(d, "appearance_id", limit=10**9)
        n_picks = len(picks)
        users = d.get("draft_entries") or []
        n_entries = len(users) if users else len({p.get("draft_entry_id") for p in picks})
        rounds = n_picks / n_entries if n_entries else float("nan")
        print(f"  draft {str(d.get('id'))[:8]}  state={d.get('draft_state')}  "
              f"picks={n_picks}  entries={n_entries}  rounds={rounds:.0f}  "
              f"tournament_id={str(d.get('tournament_id'))[:8]}")

        # First 12 picks by position + per-team QB counts
        pos_of_pick = []
        qb_per_entry = Counter()
        for p in sorted(picks, key=lambda p: p.get("number", 0)):
            ap = p.get("appearance_id")
            pid = appearances.get(ap)
            pos = players.get(pid, ("", "", "?"))[2] if pid else "?"
            pos_of_pick.append(pos)
            if pos == "QB":
                qb_per_entry[p.get("draft_entry_id")] += 1
        print(f"    first 12 picks by position: {pos_of_pick[:12]}")
        first_qb = next((i + 1 for i, p in enumerate(pos_of_pick) if p == "QB"), None)
        print(f"    first QB taken at overall pick: {first_qb}")
        qb_counts = Counter(qb_per_entry.values())
        print(f"    QBs per team distribution: {dict(sorted(qb_counts.items()))}")

# ---------------------------------------------------------------------------
# Any slot/lineup metadata in the export?
# ---------------------------------------------------------------------------
print("\n== slot/lineup metadata search ==")
slot_hits = walk_find(data, "slot", limit=3) + walk_find(data, "slots", limit=3) + \
    walk_find(data, "lineup", limit=3)
for h in slot_hits[:3]:
    print(json.dumps(h, indent=1)[:500])
if not slot_hits:
    print("  (no slot/lineup keys captured in the export)")
