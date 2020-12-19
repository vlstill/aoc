from fja.lib.grammars_cfg import CFG, CachedCYK
from fja.lib.parsing.parser import Parser
import sys
from typing import List, Optional

grammar_rules: List[str] = []
cfg: Optional[CFG]
todo: List[str] = []
parser = Parser()

part = 0
for line in map(str.rstrip, sys.stdin):
    if part == 0:
        if line == "":
            part = 1
            g = "\n".join(grammar_rules)
            cfg = parser.str_to_cfg(g)
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
        todo.append(line)


assert cfg is not None


def cound_prods(grm: CFG) -> None:
    cyk = CachedCYK(grm)
    count = 0
    for line in todo:
        if cyk.generates(line):
            count += 1
    print(count)


def part_2() -> None:
    new_rules: List[str] = []
    for old in grammar_rules:
        if old.startswith("<8> →"):
            new_rules.append("<8> → <42> | <42> <8>")
        elif old.startswith("<11> →"):
            new_rules.append("<11> → <42> <31> | <42> <11> <31>")
        else:
            new_rules.append(old)
    g = "\n".join(new_rules)
    cound_prods(parser.str_to_cfg(g))


cound_prods(cfg)
part_2()
