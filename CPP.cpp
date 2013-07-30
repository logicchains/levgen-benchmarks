#include <inttypes.h>
#include <vector>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

const uint32_t TILE_DIM = 50;
const uint32_t MIW = 2;
const uint32_t MAXWID = 8;

class GenRandGenerator
{
public:
    explicit GenRandGenerator( int32_t seed ) :
        gen( seed )
    {
    };

    uint32_t operator()()
    {
        gen += gen;
        gen ^= 1;
        int32_t tgen=gen;
        if ( tgen < 0) {
            gen ^= 0x88888eef;
        }
        int a = gen;
        return a;
    }
private:
    uint32_t gen;
};

struct Tile
{
    uint32_t X;
    uint32_t Y;
    uint32_t T;
};

struct Room
{
public:
    Room( uint32_t x,
            uint32_t y,
            uint32_t w,
            uint32_t h,
            uint32_t roomNum ) :
                x( x ),
                y( y ),
                w( w ),
                h( h ),
                roomNum( roomNum )
    {
    }

    uint32_t x;
    uint32_t y;
    uint32_t w;
    uint32_t h;
    uint32_t roomNum;

};

class Level
{
public:
    Level()
    {
        tiles.resize( 2500 );
        for( std::size_t t = 0 ; t < 2500 ; t++ )
        {
            tiles[t].X = t % TILE_DIM;
            tiles[t].Y = t / TILE_DIM;
            tiles[t].T = 0;
        }
        rooms.reserve( 100 );
    }

    void fillTiles()
    {
        for( std::vector<Room>::iterator rIter = rooms.begin(), end = rooms.end() ; rIter != end ; ++rIter )
        {
            for( uint32_t yi = rIter->y ; yi <= ( rIter->y + rIter->h ) ; ++yi )
            {
                for( uint32_t xi = rIter->x ; xi <= ( rIter->x + rIter->w ) ; ++xi )
                {
                    uint32_t num( yi * TILE_DIM + xi );
                    tiles[num].T = 1;
                }
            }
        }
    }

    std::vector<Tile> tiles;
    std::vector<Room> rooms;
};

struct NumRoomsMetric
{
    bool isBetterLevel( Level & x, Level & y )
    {
        return y.rooms.size() > x.rooms.size();
    }
};

template <typename RandomGenerator>
class LevelGenerator
{
public:
    LevelGenerator( RandomGenerator randomGenerator ) :
        nextRandomGenerator_( randomGenerator )
    {
    }

    virtual ~LevelGenerator()
    {
    }

    void generateLevels()
    {
        levels_ = std::vector<Level>( 100 );
        for( std::size_t i = 0 ; i < 100 ; i++ )
        {
            Level level;
            for( std::size_t ii = 0 ; ii < 50000 ; ii++ )
            {
                makeRoomSilentlyFail( level );
                if( level.rooms.size() == 99 )
                {
                    break;
                }
            }
            level.fillTiles();
            levels_.push_back( level );
        }
    }

    template <typename LevelMetric>
    Level & pickLevelByCriteria( LevelMetric levelMetric )
    {
        std::vector<Level>::iterator lIter( levels_.begin() );
        std::vector<Level>::iterator lEnd( levels_.end() );
        Level & result( *lIter );
        lIter++;
        if( lIter == lEnd ) return result;

        for( ; lIter != lEnd ; ++lIter )
        {
            if( levelMetric.isBetterLevel( result, *lIter ) )
            {
                result = *lIter;
            }
        }

        return result;
    }

private:
    void makeRoomSilentlyFail( Level & level )
    {
        uint32_t r1 = nextRandomGenerator_();
        uint32_t r2 = nextRandomGenerator_();
        uint32_t x( r1 % TILE_DIM );
        uint32_t y( r2 % TILE_DIM );
        uint32_t w( (r1 % MAXWID) + MIW );
        uint32_t h( (r2 % MAXWID) + MIW );

        if( (x+w) >= TILE_DIM || (y+h) >= TILE_DIM || x == 0 || y == 0 ) return;

        if( !isCollision( level.rooms, x, y, w, h ) )
        {
            Room r( x, y, w, h, level.rooms.size() + 1 );
            level.rooms.push_back( r );
        }
    }

    bool isCollision( std::vector<Room> & rooms, uint32_t x, uint32_t y, uint32_t w, uint32_t h )
    {
        for( std::vector<Room>::iterator rIter = rooms.begin(), end = rooms.end() ; rIter != end ; ++rIter )
        {
            uint32_t & rX( rIter->x );
            uint32_t & rY( rIter->y );
            uint32_t & rW( rIter->w );
            uint32_t & rH( rIter->h );

            bool roomOkay;

            if( (rX + rW + 1 ) < x || rX > (x + w + 1 ) )
            {
                roomOkay = true;
            }
            else if( (rY + rH + 1) < y || rY > (y + h + 1 ) )
            {
                roomOkay = true;
            }
            else
            {
                roomOkay = false;
            }

            if( !roomOkay )
            {
                return true;
            }
        }
        return false;
    }

    RandomGenerator nextRandomGenerator_;

    std::vector<Level> levels_;
};

void printLevel( Level & level )
{
    for( uint32_t i = 0 ; i < 2500 ; i++ )
    {
        printf( "%d", level.tiles[i].T );
        if( i % ( TILE_DIM ) == 49 && i != 0 )
        {
            printf( "\n" );
        }
    }
}

int main(int argc, char* argv[]) {
  clock_t start, stop;
	start = clock();
	int v = atoi(argv[1]);
	printf("The random seed is: %d \n", v);
	srand(v);

    GenRandGenerator randGenerator( v );

    LevelGenerator<GenRandGenerator> levelGenerator( randGenerator );
    levelGenerator.generateLevels();

    NumRoomsMetric numRoomsMetric;
    Level & l( levelGenerator.pickLevelByCriteria( numRoomsMetric ) );
    printLevel( l );

    stop = clock();
    long clocks_per_ms = CLOCKS_PER_SEC/1000;
    printf("%ld\n", (stop - start)/clocks_per_ms);

    return 0;
}
