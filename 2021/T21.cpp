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
    std::string line;
    std::getline(std::cin, line);
    int a = line.back() - '0';
    std::getline(std::cin, line);
    int b = line.back() - '0';

    std::array<int, 2> positions{a, b};
    std::array<int, 2> scores = {0, 0};
    int dice_next = 1;
    int rolled = 0;

    auto roll = [&] {
	int val = dice_next;
	dice_next = (dice_next % 100) + 1;
	++rolled;
	return val;
    };

    while (scores[0] < 1000 && scores[1] < 1000) {
	for (int i = 0; i < 2; ++i) {
	    int move = roll() + roll() + roll();
	    positions[i] += move;
	    positions[i] = ((positions[i] - 1) % 10) + 1;
	    scores[i] += positions[i];
	    if (scores[0] >= 1000)
		break;
	}
    };

    std::cout << std::min(scores[0], scores[1]) * rolled << '\n';
}
