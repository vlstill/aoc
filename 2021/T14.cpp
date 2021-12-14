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

void step(std::list<char> &polymer, const std::map<std::string, char> inserts) {
    for (auto it = polymer.begin(), e = std::prev(polymer.end()); it != e; ++it) {
	std::string key{*it, *std::next(it)};
	if (auto found = inserts.find(key); found != inserts.end())
	    it = polymer.insert(std::next(it), found->second);
    }
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

    for (auto [k, v] : inserts)
	std::cerr << k << " → " << v << '\n';

    std::list polymer(templ.begin(), templ.end());
    for (int i = 1; i <= 10; ++i) {
	step(polymer, inserts);
    }
    std::map<char, long> counts;
    for (auto c : polymer)
	++counts[c];

    {
	long min = std::numeric_limits<long>::max(), max = 0;
	for (auto [k, v] : counts) {
	    min = std::min(v, min);
	    max = std::max(v, max);
	    std::cerr << k << " = " << v << '\n';
	}

	std::cout << max - min << '\n';
    }

    for (int i = 1; i <= 30; ++i) {
	step(polymer, inserts);
    }
    {
	long min = std::numeric_limits<long>::max(), max = 0;
	for (auto [k, v] : counts) {
	    min = std::min(v, min);
	    max = std::max(v, max);
	    std::cerr << k << " = " << v << '\n';
	}
	std::cout << max - min << '\n';
    }
}
