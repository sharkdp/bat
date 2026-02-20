// C23 digit separator tests
// These tests verify that C23 digit separators are correctly highlighted

#include <stdio.h>

int main() {
    // Decimal with digit separators
    int dec1 = 1'000'000;
    int dec2 = 2'354'202'076;
    int dec3 = 100'000;
    
    // Octal with digit separators
    int oct1 = 014'70;
    int oct2 = 01'234'567;
    
    // Hexadecimal with digit separators
    int hex1 = 0xA7'45'8C'38;
    int hex2 = 0xFF'FF'FF'FF;
    int hex3 = 0x12'34'56'78;
    
    // Binary with digit separators (C23)
    int bin1 = 0b1001'1101'0010'1100;
    int bin2 = 0B01'01'10'10;
    
    // Decimal with suffix
    unsigned long long ull = 1'000'000ULL;
    
    return 0;
}
