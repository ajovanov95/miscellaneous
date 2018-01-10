#include <ostream>

class bitset
{
    private:
        unsigned int size;
        char* bytes;

        void checkIndex(unsigned int i) const;
    public:
        bitset(unsigned int sz);
        bitset(const bitset& other);
        const bitset& operator=(const bitset& other);
        ~bitset();

        void set(unsigned int i, bool val);
        bool get(unsigned int i) const;
        void toggle(unsigned int i);

        void clear();

        unsigned int getsize() const;
};

std::ostream& operator<<(std::ostream& os, const bitset& bs);
