-- This is a single-line comment

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity SyntaxTest is
    generic (
        DATA_WIDTH : integer := 8
    );
    port (
        clk     : in  std_logic;
        rst     : in  std_logic;
        a, b    : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
        sel     : in  std_logic;
        result  : out std_logic_vector(DATA_WIDTH - 1 downto 0);
        flag    : out std_logic
    );
end SyntaxTest;

architecture Behavioral of SyntaxTest is

    signal tmp : std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal done : std_logic := '0';

    type state_type is (IDLE, LOAD, EXECUTE, DONE);
    signal state : state_type := IDLE;

begin

    process(clk, rst)
        variable i : integer := 0;
    begin
        if rst = '1' then
            tmp   <= (others => '0');
            flag  <= '0';
            state <= IDLE;

        elsif rising_edge(clk) then
            case state is
                when IDLE =>
                    if sel = '1' then
                        tmp <= a and b;
                        state <= EXECUTE;
                    else
                        tmp <= a or b;
                        state <= LOAD;
                    end if;

                when LOAD =>
                    tmp <= a xor b;
                    state <= EXECUTE;

                when EXECUTE =>
                    if i < DATA_WIDTH then
                        tmp(i) <= not tmp(i);
                        i := i + 1;
                    else
                        state <= DONE;
                    end if;

                when DONE =>
                    flag <= '1';
                    state <= IDLE;

                when others =>
                    state <= IDLE;
            end case;
        end if;
    end process;

    result <= tmp;

end Behavioral;