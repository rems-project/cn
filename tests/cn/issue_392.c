struct Chunk {
    unsigned alloc_size;
};

struct Chunk *id(struct Chunk *c) {
    return c;
}

int main() {
    struct Chunk c;
    id(&c)->alloc_size;
    return 0;
}
