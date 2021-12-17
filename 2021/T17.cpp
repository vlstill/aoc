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
#include <queue>
#include <iomanip>
#include <memory>
#include <variant>
#include <type_traits>

int main() {
    int x_min = 0, x_max = 0;
    int y_min = 0, y_max = 0;

    std::string line;
    std::getline(std::cin, line, ' ');
    auto parse_range = [&] {
	std::getline(std::cin, line, ',');
	auto dot = line.find('.');
	auto eq = line.find('=');
	auto min = std::stoi(line.substr(eq + 1, dot));
	auto max = std::stoi(line.substr(dot + 2));
	return std::pair(min, max);
    };
    std::tie(x_min, x_max) = parse_range();
    std::tie(y_min, y_max) = parse_range();

    auto go = [=](int xv, int yv) -> std::optional<int> {
	int top = 0;
	int x = 0, y = 0;

//	std::cerr << xv << ',' << yv << ": 0,0 → ";
	for (int i = 0; ; ++i) {
	    x += xv;
	    xv = std::max(0, xv - 1);
	    y += yv;
	    --yv;
	    top = std::max(top, y);

//	    std::cerr << x << ',' << y;

	    if (x_min <= x && x <= x_max && y_min <= y && y <= y_max) {
//		std::cerr << " ⊤\n";
		return top;
	    }
	    if (x > x_max || y < y_min) {
//		std::cerr << " ⊥\n";
		return std::nullopt;
	    }

//	    std::cerr << " → ";
	}
    };

    std::set<std::pair<int, std::pair<int, int>>> reach;

    // assuming the target is in x positive, y negative direct heading from 0,0
    for (int xv = 0; xv <= x_max; ++xv) {
	for (int yv = y_min; yv < 10000; ++yv) {
	    auto r = go(xv, yv);
	    if (r.has_value())
		reach.insert({*r, {xv, yv}});
	}
    }
//    for (auto [h, p] : reach) {
//	std::cerr << h << ' ' << p.first << ',' << p.second << '\n';
//    }
    std::cout << reach.rbegin()->first << '\n';
    std::cout << reach.size() << '\n';
}
