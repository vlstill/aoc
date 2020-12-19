from fja.lib.cfl import CFG
from fja.lib.parsing.parser import Parser
import sys
from typing import List, Optional

grammar_rules: List[str] = []
cfg: Optional[CFG]
count = 0

part = 0
for line in map(str.rstrip, sys.stdin):
    if part == 0:
        if line == "":
            part = 1
            parser = Parser()
            g = "\n".join(grammar_rules)
            cfg = parser.str_to_cfg(g)
            print(g)
            continue

        nt, rules = line.replace('"', '').split(":", 1)
        rules = " ".join(map(lambda x: f"<{x}>" if x.isdigit()
                             else x, rules.split(" ")))
        prod = f"<{nt}> → {rules}"
        if int(nt) == 0:
            grammar_rules.insert(0, prod)
        else:
            grammar_rules.append(prod)

    else:
        assert cfg is not None
        if cfg.generates(line):
            print(line)
            count += 1

print(count)
