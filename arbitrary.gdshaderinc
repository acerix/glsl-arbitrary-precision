const int PRECISION = 3;
const int ARRAY_SIZE = (PRECISION+1);
const float BASE = 4294967296.0;
const uint HALF_BASE = 2147483648u;

void assign(inout uint x[ARRAY_SIZE], in uint y[ARRAY_SIZE]) {
    for(int i = 0; i <= PRECISION; i++) {
        x[i] = y[i];
    }
}

void zero(inout uint x[ARRAY_SIZE]) {
    for(int i = 0; i <= PRECISION; i++) {
        x[i] = 0u;
    }
}

void load(inout uint x[ARRAY_SIZE], in float v) {
    float load_value = v;
    if (load_value < 0.0) {
        x[0] = 1u;
        load_value *= -1.0;
    } else {
        x[0] = 0u;
    }
    for(int i = 1; i <= PRECISION; i++) {
        x[i] = uint(load_value);
        load_value -= float(x[i]);
        load_value *= BASE;
    }
}

void shift(inout uint x[ARRAY_SIZE], in int v) {
    int shift_n = v;
    for(int i = shift_n + 1; i <= PRECISION; i++) {
        x[i] = x[i - shift_n];
    }
    for(int i = 1; i <= shift_n; i++) {
        x[i] = 0u;
    }
}

void negate(inout uint x[ARRAY_SIZE]) {
    x[0] = (x[0] == 0u ? 1u : 0u);
}

void add(in uint a[ARRAY_SIZE], in uint b[ARRAY_SIZE], inout uint r[ARRAY_SIZE]) {
    uint add_buffer[ARRAY_SIZE];
    bool add_pa = a[0] == 0u;
    bool add_pb = b[0] == 0u;
    if (add_pa == add_pb) {
        uint add_carry = 0u;
        for(int i = PRECISION; i > 0; i--) {
            uint add_next = 0u;
            if(a[i] + b[i] < a[i]) {
                add_next = 1u;
            }
            add_buffer[i] = a[i] + b[i] + add_carry;
            add_carry = add_next;
        }
        if(!add_pa) {
            add_buffer[0] = 1u;
        } else {
            add_buffer[0] = 0u;
        }
    } else {
        bool add_flip = false;
        for(int i = 1; i <= PRECISION; i++) {
            if(b[i] > a[i]) {
                add_flip = true;
                break;
            }
            if(a[i] > b[i]) {
                break;
            }
        }
        if(add_flip) {
            uint add_borrow = 0u;
            for(int i = PRECISION; i > 0; i--) {
                add_buffer[i] = b[i] - a[i] - add_borrow;
                if(b[i] < a[i] + add_borrow) {
                    add_borrow = 1u;
                } else {
                    add_borrow = 0u;
                }
            }
        } else {
            uint add_borrow = 0u;
            for(int i = PRECISION; i > 0; i--) {
                add_buffer[i] = a[i] - b[i] - add_borrow;
                if(a[i] < b[i] || a[i] < b[i] + add_borrow) {
                    add_borrow = 1u;
                } else {
                    add_borrow = 0u;
                }
            }
        }
        if(add_pa == add_flip) {
            add_buffer[0] = 1u;
        } else {
            add_buffer[0] = 0u;
        }
    }
    assign(r, add_buffer);
}

void mul(in uint a[ARRAY_SIZE], in uint b[ARRAY_SIZE], inout uint r[ARRAY_SIZE]) {
    uint mul_buffer[ARRAY_SIZE];
    zero(mul_buffer);
    uint mul_product[2 * PRECISION - 1];
    for(int i = 0; i < 2 * PRECISION - 1; i++) {
        mul_product[i] = 0u;
    }
    for(int i = 0; i < PRECISION; i++) {
        uint mul_carry = 0u;
        for(int j = 0; j < PRECISION; j++) {
            uint mul_next = 0u;
            uint mul_value = a[PRECISION - i] * b[PRECISION - j];
            if(mul_product[i + j] + mul_value < mul_product[i + j]) {
                mul_next++;
            }
            mul_product[i + j] += mul_value;
            if(mul_product[i + j] + mul_carry < mul_product[i + j]) {
                mul_next++;
            }
            mul_product[i + j] += mul_carry;
            uint mul_lower_a = a[PRECISION - i] & 0xFFFFu;
            uint mul_upper_a = a[PRECISION - i] >> 16u;
            uint mul_lower_b = b[PRECISION - j] & 0xFFFFu;
            uint mul_upper_b = b[PRECISION - j] >> 16u;
            uint mul_lower = mul_lower_a * mul_lower_b;
            uint mul_upper = mul_upper_a * mul_upper_b;
            uint mul_mid = mul_lower_a * mul_upper_b;
            mul_upper += mul_mid >> 16u;
            mul_mid = mul_mid << 16u;
            if(mul_lower + mul_mid < mul_lower) {
                mul_upper++;
            }
            mul_lower += mul_mid;
            mul_mid = mul_lower_b * mul_upper_a;
            mul_upper += mul_mid >> 16u;
            mul_mid = mul_mid << 16u;
            if(mul_lower + mul_mid < mul_lower) {
                mul_upper++;
            }
            mul_carry = mul_upper + mul_next;
        }
        if(i + PRECISION < 2 * PRECISION - 1) {
            mul_product[i + PRECISION] += mul_carry;
        }
    }
    if(PRECISION > 1 && mul_product[PRECISION - 2] >= HALF_BASE) {
        for(int i = PRECISION - 1; i < 2 * PRECISION - 1; i++) {
            if(mul_product[i] + 1u > mul_product[i]) {
                mul_product[i]++;
                break;
            }
            mul_product[i]++;
        }
    }
    for(int i = 0; i < PRECISION; i++) {
        mul_buffer[i + 1] = mul_product[2 * PRECISION - 2 - i];
    }
    if((a[0] == 0u) != (b[0] == 0u)) {
        mul_buffer[0] = 1u;
    }
    assign(r, mul_buffer);
}