def amicable_numbers_sum():
    sum = 0
    for a in range(1, 10000):
        sum_a = 0
        for division in range(1, a // 2 + 1):
            if a % division == 0:
                sum_a += division

        sum_b = 0
        for division in range(1, sum_a // 2 + 1):
            if sum_a % division == 0:
                sum_b += division

        if a == sum_b:
            sum += a
            sum += sum_a
            
    return sum


if __name__ == "__main__":
    print(amicable_numbers_sum())