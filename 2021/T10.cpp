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


std::map< char, int > points = { {')', 3 },
                                 {']', 57 },
                                 {'}', 1197 },
                                 {'>', 25137 } };

int main() {
    std::string line;
    long score = 0;
    while (std::getline(std::cin, line)) {
        std::vector< char > stack;
        for (char c : line) {
            if (close.contains(c)) {
                stack.push_back(close[c]);
            }
            else {
                char top = stack.back();
                stack.pop_back();
                if (top != c) {
                    score += points[c];
                }
            }
        }
    }
    std::cout << score << '\n';
}
