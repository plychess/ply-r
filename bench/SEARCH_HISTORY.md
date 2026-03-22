# Search Optimization History

## Baseline — Raw Alpha-Beta (ab64862)
- Move ordering: MVV-LVA captures first
- Quiescence search: captures only
- Iterative deepening: yes
- Check extensions: yes
- No TT, no null move, no LMR

| Position | Depth | Move | Nodes | Time(s) | NPS |
|---|---|---|---|---|---|
| Starting pos | 7 | Nc3 | 18,622,989 | 2.5s | 7.5M |
| Kiwipete | 6 | Ba6 | 978,047,849 | 190.4s | 5.1M |
| Italian | 6 | Bg5 | 288,805,621 | 44.0s | 6.6M |
| Mate in 2 | 6 | Qf7 | 1,616,245 | 0.2s | 8.1M |
| Endgame | 8 | Rxf4 | 2,873,292 | 0.3s | 10.7M |
| **TOTAL** | | | **1,289,965,996** | **237.3s** | **5.4M** |

## Optimization 1 — TT + Null Move Pruning + LMR + Delta Pruning
- Search TT: 1M entries (~24MB), exact/alpha/beta flags
- Null move pruning: R=2 (R=3 at depth≥6), skip when in check
- Late move reductions: reduce quiet moves after first 4 by 1-2 plies
- Delta pruning in quiescence: skip captures that can't raise alpha
- TT move ordering: TT best move searched first

| Position | Depth | Move | Nodes | Time(s) | NPS | Speedup |
|---|---|---|---|---|---|---|
| Starting pos | 7 | Nc3 | 37,216 | 0.03s | 1.4M | **93x fewer nodes** |
| Kiwipete | 6 | Ba6 | 72,576,963 | 29.3s | 2.5M | **13x fewer nodes** |
| Italian | 6 | Bg5 | 1,231,504 | 2.4s | 0.5M | **234x fewer nodes** |
| Mate in 2 | 6 | Qf7 | 19,940 | 0.005s | 3.9M | **81x fewer nodes** |
| Endgame | 8 | Rxf4 | 38,765 | 0.01s | 3.8M | **74x fewer nodes** |
| **TOTAL** | | | **73,904,388** | **31.7s** | **2.3M** | **7.5x faster** |

## Optimization 2 — PVS + Killers + History + Aspiration + Reverse Futility
- Principal Variation Search: null window for non-PV moves, re-search on fail high
- Killer moves: 2 slots per ply, quiet moves that caused beta cutoffs
- History heuristic: score quiet cutoff moves by depth², used for ordering
- Aspiration windows: ±50cp around previous depth score, widen on fail
- Reverse futility pruning: skip search at depth≤3 when eval far above beta
- Larger TT: 4M entries (~96MB)

| Position | Depth | Move | Nodes | Time(s) | vs Baseline | vs Opt1 |
|---|---|---|---|---|---|---|
| Starting pos | 7 | d4 | 33,585 | 0.03s | **555x** | 1.1x |
| Kiwipete | 6 | dxe6 | 21,283,043 | 9.1s | **21x** | 3.4x |
| Italian | 6 | Bg5 | 271,117 | 0.6s | **73x** | 4.5x |
| Mate in 2 | 6 | Qf7 | 10,657 | 0.002s | **40x** | 1.9x |
| Endgame | 8 | Rxf4 | 23,619 | 0.008s | **36x** | 1.6x |
| **TOTAL** | | | **21,622,021** | **9.7s** | **24x faster** | **3.3x faster** |

### Max reachable depths (30s time limit)

| Position | Baseline | Opt 1 (TT+NMP+LMR) | Opt 2 (PVS+Killer+History) |
|---|---|---|---|
| Starting pos | d7 (2.5s) | d12 (4.6s) | **d12 (2.8s)** |
| Kiwipete | d5 (timeout) | d7 (33s) | **d8 (69s)** |
| Italian | d5 (timeout) | d8 (29s) | **d11 (21s)** |

## Optimization 3 — Lazy SMP (multi-threaded search)
- All threads search the full tree independently with shared TT
- Main thread searches at target depth; helpers at ±1, ±2 offset depths
- Per-thread killer moves and history tables
- Atomic node counter
- 12 threads on Apple M-series

| Position | Depth | Move | Nodes | Time(s) | vs Baseline | vs Opt2 |
|---|---|---|---|---|---|---|
| Starting pos | 7 | Nc3 | 137,270 | 0.02s | **135x** | ~same |
| Kiwipete | 6 | Ba6 | 77,510,009 | 6.1s | **31x** | 1.5x |
| Italian | 6 | Bg5 | 12,615,278 | 1.3s | **34x** | 1.8x |
| Mate in 2 | 6 | Qf7 | 107,697 | 0.01s | **20x** | ~same |
| Endgame | 8 | Rxf4 | 200,577 | 0.02s | **15x** | ~same |
| **TOTAL** | | | **90,570,831** | **7.5s** | **32x faster** | NPS 12.2M vs 2.3M |

### Max reachable depths (30s time limit)

| Position | Baseline | Opt2 (single) | Opt3 (Lazy SMP) |
|---|---|---|---|
| Starting pos | d7 (2.5s) | d12 (2.8s) | **d12 (1.7s)** |
| Kiwipete | d5 (timeout) | d8 (69s) | **d8 (29s)** |
| Italian | d5 (timeout) | d11 (21s) | **d11 (26s)** |

### Overall progression

| Version | Benchmark time | NPS | Total speedup |
|---|---|---|---|
| Baseline (raw alpha-beta) | 237.3s | 5.4M | 1x |
| +TT +NMP +LMR | 31.7s | 2.3M | 7.5x |
| +PVS +Killers +History +Aspiration | 9.7s | 2.2M | 24x |
| +Lazy SMP (12 threads) | 7.5s | 12.2M | **32x** |
