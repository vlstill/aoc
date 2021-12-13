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
    long path_with_revisit_count = 0;

    auto go = [&](auto go, int start, int end, std::set<int> visited,
                  std::optional<int> revisit) -> void
    {
        if (start == end) {
            path_with_revisit_count++;
            if (!revisit.has_value())
                path_count++;
            return;
        }

        if (small[start]) {
            if (visited.contains(start)) {
                if (start <= 1 || revisit.has_value())
                    return;
                revisit = start;
            }
            visited.insert(start);
        }
        for (auto succ : successors[start]) {
            go(go, succ, end, visited, revisit);
        }
    };
    go(go, 0, 1, {}, std::nullopt);

    std::cout << path_count << '\n';
    std::cout << path_with_revisit_count << '\n';
}
