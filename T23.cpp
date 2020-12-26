#include <iostream>
#include <vector>
#include <list>
#include <cassert>
#include <string>
#include <stdint.h>


int main() {
    constexpr int32_t TOTAL = 1'000'000;
    constexpr int32_t ITERS = 10'000'000;
    auto sub1 = [=]( int32_t val ) { return ((TOTAL + val - 2) % TOTAL) + 1; };

    std::string cups;
    std::cin >> cups;

    std::list< int32_t > cupVals;
    using cup_iterator = std::list< int32_t >::const_iterator;
    std::vector< cup_iterator > cupPositions;
    cupPositions.resize( TOTAL + 1 );

    int32_t maxFound = 0;
    for ( auto v : cups ) {
        int32_t val = -( '0' - v );
        cupVals.push_back( val );
        cupPositions[ val ] = std::prev( cupVals.end() );
        maxFound = std::max( maxFound, val );
    }
    for ( int32_t i = maxFound + 1; i <= TOTAL; ++i ) {
        cupVals.push_back( i );
        cupPositions[ i ] = std::prev( cupVals.end() );
    }

    for ( size_t iter = 0; iter < ITERS; ++iter ) {
        auto head = cupVals.front();
        auto moved_start = std::next( cupVals.begin() );
        auto moved_end = std::next( moved_start, 3 );

        int32_t next = sub1( head );
        for ( ; std::find( moved_start, moved_end, next ) != moved_end; next = sub1( next ) )
        { }
        cupVals.splice( std::next( cupPositions[ next ] ), cupVals, moved_start, moved_end );
        cupVals.splice( cupVals.end(), cupVals, cupVals.begin(), std::next( cupVals.begin() ) );
    }

    for ( int i = 0; i < 2; ++ i ) {
        cupVals.push_back( *std::next( cupVals.begin(), i ) );
    }
    auto one = cupPositions[ 1 ];
    std::cerr << int64_t( *std::next( one ) ) * int64_t( *std::next( one, 2 ) ) << std::endl;
}
