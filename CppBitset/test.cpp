#include "bitset.hpp"
#include <iostream>
#include <exception>

int main()
{
    try
    {
      bitset bs(16);

      bs.clear();

      bs.set(14, 1);

      std::cout << bs.get(14) << std::endl;

      std::cout << bs << std::endl;
    }
    catch(std::exception ex)
    {
        std::cerr << ex.what() << std::endl;
    }

    return 0;
}
