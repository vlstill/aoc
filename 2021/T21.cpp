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

struct Game {
    Game(int a, int b) : positions{a, b} { }

    std::array<int, 2> positions;
    std::array<int, 2> scores = {0, 0};
    int dice_next = 1;
    int rolled = 0;

    int roll_det() {
	int val = dice_next;
	dice_next = (dice_next % 100) + 1;
	++rolled;
	return val;
    }

    void move(int i, int val) {
	positions[i] += val;
	positions[i] = ((positions[i] - 1) % 10) + 1;
	scores[i] += positions[i];
    }
};

const std::array< long, 10 > three_dices_freq = []{
	std::array< long, 10 > out = { 0 };
	for (int a : {1, 2, 3}) {
	    for (int b : {1, 2, 3}) {
		for (int c : {1, 2, 3}) {
		    out[a + b + c]++;
		}
	    }
	};
	assert(std::accumulate(out.begin(), out.end(), 0, [](auto a, auto b) { return a + b; }) == 27);
	return out;
    }();

void play(Game g, int max, auto &wins, long m = 1) {
    for (int a = 3; a <= 9; ++a) {
	Game copy_a(g);
	copy_a.move(0, a);
	auto a_multi = three_dices_freq[a];
	if (copy_a.scores[0] >= max) {
	    wins[0] += a_multi * m;
	    continue;
	}
	for (int b = 3; b <= 9; ++b) {
	    auto copy_b(copy_a);
	    copy_b.move(1, b);
	    auto b_multi = three_dices_freq[b];
	    if (copy_b.scores[1] >= max) {
		wins[1] += a_multi * b_multi * m;
		continue;
	    }
	    play(copy_b, max, wins, m * a_multi * b_multi);
	}
    }
}

void part_1(Game g) {
    while (g.scores[0] < 1000 && g.scores[1] < 1000) {
	for (int i = 0; i < 2; ++i) {
	    g.move(i, g.roll_det() + g.roll_det() + g.roll_det());
	    if (g.scores[0] >= 1000)
		break;
	}
    }
    std::cout << std::min(g.scores[0], g.scores[1]) * g.rolled << '\n';
}

void part_2(Game g) {
    std::array<long, 2> wins = {0, 0};
    play(g, 21, wins);
    std::cout << std::max(wins[0], wins[1]) << '\n';
}

int main() {
    std::string line;
    std::getline(std::cin, line);
    int a = line.back() - '0';
    std::getline(std::cin, line);
    int b = line.back() - '0';

    Game g(a, b);
    part_1(g);
    part_2(g);
}
