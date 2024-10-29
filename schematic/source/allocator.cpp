// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/allocator.h"

static constinit schematic::NewDeleteAllocator default_allocator;

[[nodiscard]] static constexpr size_t AlignTo(size_t value, size_t align) noexcept
{
    size_t mask = align - 1;

    const size_t unaligned = value & mask;
    if (unaligned != 0)
        value += align - unaligned;

    return value;
}

namespace schematic
{
    struct ArenaAllocator::BlockHeader
    {
        BlockHeader* previous = nullptr;
        BlockHeader* next = nullptr;
        size_t size = 0;
    };

    ArenaAllocator::ArenaAllocator() noexcept
        : ArenaAllocator(default_allocator)
    {
    }

    ArenaAllocator::~ArenaAllocator()
    {
        BlockHeader* block = static_cast<BlockHeader*>(block_);
        while (block != nullptr)
        {
            BlockHeader* const previous = block->previous;
            alloc_->Deallocate(block, block->size);
            block = previous;
        }
    }

    // NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
    void* ArenaAllocator::Allocate(size_t size, size_t align)
    {
        head_ = AlignTo(head_, align);

        if (capacity_ < size || head_ >= capacity_ - size)
        {
            EnsureBlock(size);
            head_ = AlignTo(head_, align);
        }

        void* const address = static_cast<char*>(block_) + head_;
        head_ += size;
        return address;
    }

    const char* ArenaAllocator::NewString(std::string_view string)
    {
        if (string.empty())
            return "";

        char* const memory = static_cast<char*>(Allocate(string.size() + 1 /*NUL*/, 1));
        std::memcpy(memory, string.data(), string.size());
        memory[string.size()] = '\0';
        return memory;
    }

    void ArenaAllocator::Clear()
    {
        block_ = first_;
        head_ = 0;
        capacity_ = block_ != nullptr ? static_cast<BlockHeader*>(block_)->size : 0;
    }

    void ArenaAllocator::EnsureBlock(size_t minimum)
    {
        constexpr size_t block_size = 16ull * 1024ull;
        constexpr size_t block_capacity = block_size - sizeof(BlockHeader);

        const bool is_large = minimum > block_capacity;

        void* block = nullptr;
        if (block_ != nullptr && !is_large && static_cast<BlockHeader*>(block_)->next != nullptr)
        {
            block = static_cast<BlockHeader*>(block_)->next;
        }
        else
        {
            capacity_ = block_capacity >= minimum ? block_capacity : minimum;
            block = alloc_->Allocate(sizeof(BlockHeader) + capacity_);
            head_ = sizeof(BlockHeader);
        }

        BlockHeader* const header = new (block) BlockHeader{ .previous = static_cast<BlockHeader*>(block_), .size = capacity_ };

        if (block_ != nullptr)
            static_cast<BlockHeader*>(block_)->next = header;
        if (first_ == nullptr)
            first_ = header;

        block_ = block;
    }
} // namespace schematic
