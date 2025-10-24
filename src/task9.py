def find_triple_cycle():
    for n in range(25):
        for m in range(25):
            if 2 * m ** 2 + 2 * m * n == 1000:
                return (m ** 2 - n ** 2) * 2 * m * n * (m ** 2 + n ** 2)
            

if __name__ == "__main__":
    print(find_triple_cycle())