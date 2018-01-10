#include "bitset.hpp"
#include <exception>
#include <stdexcept>

bitset::bitset(unsigned int sz) : size(sz)
{
    size  = sz / 8;

    bytes = new char[size];
}

bitset::bitset(const bitset& other)
{
    size = other.size;
    bytes = other.bytes;
}

const bitset& bitset::operator=(const bitset& other)
{
    size = other.size;
    bytes = other.bytes;
}

bitset::~bitset()
{
    delete[] bytes;
}

void bitset::checkIndex(unsigned int i) const
{
    if (i >= size)
      throw std::out_of_range("Index out of bounds");
}

void bitset::set(unsigned int index, bool val)
{
    try
    {
        checkIndex(index);

        int k = index / 8;
        int r = index % 8;

        if (val == false)
          bytes[k] = bytes[k] & ~(1 << r);
        else
          bytes[k] = bytes[k] | (1 << r);
    }
    catch(std::exception ex)
    {
        throw ex;
    }
}

bool bitset::get(unsigned int index) const
{
  try
  {
      checkIndex(index);

      int k = index / 8;
      int r = index % 8;

      char b = bytes[k] & (1 << r);

      return b == 0 ? false : true;
  }
  catch(std::exception ex)
  {
      throw ex;
  }
}

void bitset::toggle(unsigned int index)
{

}

void bitset::clear()
{
      for (int i = 0; i < size; i++)
        bytes[i] = bytes[i] ^ bytes[i];
}

unsigned int bitset::getsize() const
{
    return size * 8;
}

std::ostream& operator<<(std::ostream& os, const bitset& bs)
{
    for (int i = 0; i < bs.getsize(); i++)
    {
        bool bit = bs.get(i);

        os << ((bit == true) ? "1" : "0");

        if (i % 8 == 0)
          os << " ";
    }

    return os;
}
