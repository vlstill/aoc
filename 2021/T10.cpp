#include <cassert>
#include <iostream>
#include <iterator>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

std::map< char, char > close = { {'(', ')'},
                                 {'[', ']'},
                                 {'{', '}'},
                                 {'<', '>'} };


std::map< char, int > points_err = { {')', 3 },
                                     {']', 57 },
                                     {'}', 1197 },
                                     {'>', 25137 } };
std::map< char, int > points_complete = { {')', 1 },
                                          {']', 2 },
                                          {'}', 3 },
                                          {'>', 4 } };

int main() {
    std::string line;
    long score_err = 0;
    std::vector< long > scores_complete;
    while (std::getline(std::cin, line)) {
        std::vector< char > stack;
        bool corrupted = false;
        for (char c : line) {
            if (close.contains(c)) {
                stack.push_back(close[c]);
            }
            else {
                char top = stack.back();
                stack.pop_back();
                if (top != c) {
                    score_err += points_err[c];
                    corrupted = true;
                    break;
                }
            }
        }
        if (!corrupted) {
            long score_complete = 0;
            for (auto it = stack.rbegin(), e = stack.rend(); it != e; ++it) {
                score_complete *= 5;
                score_complete += points_complete[*it];
            }
            scores_complete.push_back(score_complete);
        }
    }
    std::cout << score_err << '\n';
    std::sort(scores_complete.begin(), scores_complete.end());
    std::cout << scores_complete[scores_complete.size() / 2] << '\n';
}
