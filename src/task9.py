def find_triple_cycle():
    for i in range(500):
        for j in range(500):
            for k in range(500):
                if i + j + k == 1000 and i ** 2 + j ** 2 == k ** 2:
                    return i * j * k

if __name__ == "__main__":
    print(find_triple_cycle())