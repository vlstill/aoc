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

int main() {
    std::string line;
    std::map<std::string, int> numbers = { { "start", 0 }, { "end", 1 } };
    std::vector<bool> small = { true, true };
    std::vector<std::vector<int>> successors;
    while (std::getline(std::cin, line)) {
        auto split = line.find('-');
        auto from = line.substr(0, split);
        auto to = line.substr(split + 1);
        auto [fit, i₁] = numbers.emplace(from, numbers.size());
        auto [tit, i₂] = numbers.emplace(to, numbers.size());
        auto last = std::max(fit->second, tit->second);
        if (successors.size() <= last) {
            successors.resize(last + 1);
            small.resize(last + 1, false);
        }
        for (auto vec : {from, to}) {
            if ('a' <= vec[0] && vec[0] <= 'z') {
                small[numbers[vec]] = true;
            }
        }
        successors[fit->second].push_back(tit->second);
        successors[tit->second].push_back(fit->second);
    }
    for ( auto [vec, num] : numbers ) {
        std::cerr << vec << " = " << num << " → ";
        for (auto succ : successors[num]) {
            std::cerr << succ << 's' << small[succ] << ' ';
        }
        std::cerr << '\n';
    }

    long path_count = 0;

    auto go = [&](auto go, int start, int end, std::set<int> visited, std::vector<int> trace) -> void {
        trace.push_back(start);
        if (start == end) {
            std::copy(trace.begin(), trace.end(), std::ostream_iterator<int>(std::cerr, " → "));
            std::cerr << '\n';
            path_count++;
            return;
        }

        if (small[start]) {
            if (visited.contains(start))
                return;
            visited.insert(start);
        }
        for (auto succ : successors[start]) {
            go(go, succ, end, visited, trace);
        }
    };
    go(go, 0, 1, {}, {});

    std::cout << path_count << '\n';
}
