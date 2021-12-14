#include <cassert>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <iterator>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>
#include <deque>
#include <optional>
#include <list>
#include <limits>

using PolyPairs = std::map<std::string, long>;


PolyPairs step(const PolyPairs &polypairs, const std::map<std::string, char> inserts) {
    PolyPairs copy(polypairs);
    for (auto &[pair, cnt] : polypairs) {
	if (auto found = inserts.find(pair); found != inserts.end()) {
	    auto c = found->second;
	    copy[pair] -= cnt;
	    std::string fst{pair[0], c}, snd{c, pair[1]};
	    copy[fst] += cnt;
	    copy[snd] += cnt;
	}
    }
    /*
    std::vector<std::string> drop;
    for (auto &[p, cnt] : polypairs) {
	if (cnt == 0)
	    drop.push_back(p);
    }
    for (auto &p : drop)
	copy.erase(p);
	*/
    return copy;
}

void dump(PolyPairs &pp) {
    for (auto &[p, c] : pp) {
	std::cerr << p << c;
    }
    std::cerr << '\n';
}

int main() {
    std::string templ;
    std::getline(std::cin, templ);

    std::string line;
    std::getline(std::cin, line);
    std::map<std::string, char> inserts;
    while (std::getline(std::cin, line)) {
	std::string src;
	std::stringstream ss(line);
	ss >> src;
	inserts[src] = line.back();
    }

    std::map<std::string, long> polypairs;
    for (auto it = templ.begin(), e = std::prev(templ.end()); it != e; ++it) {
	polypairs[std::string{*it, *std::next(it)}] = 1;
    }

    auto do_pairs = [&]() {
	std::map<char, long> counts;
	for (auto [k, v] : polypairs) {
	    counts[k[0]] += v;
	    counts[k[1]] += v;
	}
	counts[templ.front()]++;
	counts[templ.back()]++;

	{
	    long min = std::numeric_limits<long>::max(), max = 0;
	    for (auto [k, v] : counts) {
		min = std::min(v, min);
		max = std::max(v, max);
	    }

	    std::cout << max / 2 - min / 2 << '\n';
	}
    };

    for (int i = 1; i <= 40; ++i) {
	polypairs = step(polypairs, inserts);
	if (i == 10)
	    do_pairs();
    }
    do_pairs();

}
