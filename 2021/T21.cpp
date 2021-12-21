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

    /*
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
    */
};

std::array< long, 10 > three_dices_freq = { 0 };

void play(Game g, int max, auto &wins, long m = 1) {
    for (int a = 3; a <= 9; ++a) {
	Game copy_a(g);
	copy_a.move(0, a);
	auto a_multi = three_dices_freq[a];
//	std::cerr << "A " << a << " (" << a_multi << ',' << copy_a.positions[0] << ") ";
	if (copy_a.scores[0] >= max) {
	    wins[0] += a_multi * m;
//	    std::cerr << copy_a.scores[0] << " m " << a_multi * m << " w1\n";
	    continue;
	}
	for (int b = 3; b <= 9; ++b) {
	    auto copy_b(copy_a);
	    copy_b.move(1, b);
	    auto b_multi = three_dices_freq[b];
//	    std::cerr << "B " << b << " (" << b_multi << ',' << copy_b.positions[1] << "): ";
	    if (copy_b.scores[1] >= max) {
		wins[1] += a_multi * b_multi * m;
//		std::cerr << copy_b.scores[1] << " m " << a_multi * b_multi * b << " w2\n";
		continue;
	    }
//	    std::cerr << m * a_multi * b_multi << '\n';
	    play(copy_b, max, wins, m * a_multi * b_multi);
	}
    }
}

int main() {
//    std::cout << std::min(scores[0], scores[1]) * rolled << '\n';
    std::string line;
    std::getline(std::cin, line);
    int a = line.back() - '0';
    std::getline(std::cin, line);
    int b = line.back() - '0';

    Game g(a, b);
    std::array<long, 2> wins = {0, 0};

    for (int a : {1, 2, 3}) {
	for (int b : {1, 2, 3}) {
	    for (int c : {1, 2, 3}) {
		three_dices_freq[a + b + c]++;
	    }
	}
    };
    assert(std::accumulate(three_dices_freq.begin(), three_dices_freq.end(), 0, [](auto a, auto b) { return a + b; }) == 27);

    play(g, 21, wins);
    std::cerr << wins[0] << ' ' << wins[1] << '\n';
    std::cout << std::max(wins[0], wins[1]) << '\n';
}
