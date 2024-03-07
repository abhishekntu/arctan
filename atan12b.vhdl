--------------------------------------------------------------------------------
--                             LZOC_10_F400_uid4
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007)
--------------------------------------------------------------------------------
-- Pipeline depth: 2 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity LZOC_10_F400_uid4 is
   port ( clk, rst : in std_logic;
          I : in  std_logic_vector(9 downto 0);
          OZB : in  std_logic;
          O : out  std_logic_vector(3 downto 0)   );
end entity;

architecture arch of LZOC_10_F400_uid4 is
signal sozb, sozb_d1, sozb_d2 :  std_logic;
signal level4, level4_d1 :  std_logic_vector(15 downto 0);
signal digit4, digit4_d1 :  std_logic;
signal level3 :  std_logic_vector(7 downto 0);
signal digit3, digit3_d1 :  std_logic;
signal level2, level2_d1 :  std_logic_vector(3 downto 0);
signal digit2 :  std_logic;
signal level1 :  std_logic_vector(1 downto 0);
signal digit1 :  std_logic;
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            sozb_d1 <=  sozb;
            sozb_d2 <=  sozb_d1;
            level4_d1 <=  level4;
            digit4_d1 <=  digit4;
            digit3_d1 <=  digit3;
            level2_d1 <=  level2;
         end if;
      end process;
   sozb <= OZB;
   level4<= I& (5 downto 0 => not(sozb));
   ----------------Synchro barrier, entering cycle 1----------------
   digit4<= '1' when level4_d1(15 downto 8) = (15 downto 8 => sozb_d1) else '0';
   level3<= level4_d1(7 downto 0) when digit4='1' else level4_d1(15 downto 8);
   digit3<= '1' when level3(7 downto 4) = (7 downto 4 => sozb_d1) else '0';
   level2<= level3(3 downto 0) when digit3='1' else level3(7 downto 4);
   ----------------Synchro barrier, entering cycle 2----------------
   digit2<= '1' when level2_d1(3 downto 2) = (3 downto 2 => sozb_d2) else '0';
   level1<= level2_d1(1 downto 0) when digit2='1' else level2_d1(3 downto 2);
   digit1<= '1' when level1(1 downto 1) = (1 downto 1 => sozb_d2) else '0';
   O <= digit4_d1 & digit3_d1 & digit2 & digit1;
end architecture;

--------------------------------------------------------------------------------
--                     LeftShifter_11_by_max_10_F400_uid8
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2011)
--------------------------------------------------------------------------------
-- Pipeline depth: 1 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity LeftShifter_11_by_max_10_F400_uid8 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(10 downto 0);
          S : in  std_logic_vector(3 downto 0);
          R : out  std_logic_vector(20 downto 0)   );
end entity;

architecture arch of LeftShifter_11_by_max_10_F400_uid8 is
signal level0 :  std_logic_vector(10 downto 0);
signal ps, ps_d1 :  std_logic_vector(3 downto 0);
signal level1 :  std_logic_vector(11 downto 0);
signal level2 :  std_logic_vector(13 downto 0);
signal level3, level3_d1 :  std_logic_vector(17 downto 0);
signal level4 :  std_logic_vector(25 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            ps_d1 <=  ps;
            level3_d1 <=  level3;
         end if;
      end process;
   level0<= X;
   ps<= S;
   level1<= level0 & (0 downto 0 => '0') when ps(0)= '1' else     (0 downto 0 => '0') & level0;
   level2<= level1 & (1 downto 0 => '0') when ps(1)= '1' else     (1 downto 0 => '0') & level1;
   level3<= level2 & (3 downto 0 => '0') when ps(2)= '1' else     (3 downto 0 => '0') & level2;
   ----------------Synchro barrier, entering cycle 1----------------
   level4<= level3_d1 & (7 downto 0 => '0') when ps_d1(3)= '1' else     (7 downto 0 => '0') & level3_d1;
   R <= level4(20 downto 0);
end architecture;

--------------------------------------------------------------------------------
--                        GenericTable_6_16_F400_uid16
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity GenericTable_6_16_F400_uid16 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(15 downto 0)   );
end entity;

architecture arch of GenericTable_6_16_F400_uid16 is
signal TableOut :  std_logic_vector(15 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "0111111100001111" when "000000",
   "0111110100011111" when "000001",
   "0111101100111101" when "000010",
   "0111100101101001" when "000011",
   "0111011110100011" when "000100",
   "0111010111101010" when "000101",
   "0111010000111101" when "000110",
   "0111001010011101" when "000111",
   "0111000100001000" when "001000",
   "0110111101111110" when "001001",
   "0110110111111111" when "001010",
   "0110110010001001" when "001011",
   "0110101100011110" when "001100",
   "0110100110111100" when "001101",
   "0110100001100011" when "001110",
   "0110011100010011" when "001111",
   "0110010111001011" when "010000",
   "0110010010001011" when "010001",
   "0110001101010010" when "010010",
   "0110001000100010" when "010011",
   "0110000011111000" when "010100",
   "0101111111010110" when "010101",
   "0101111010111010" when "010110",
   "0101110110100101" when "010111",
   "0101110010010110" when "011000",
   "0101101110001101" when "011001",
   "0101101010001010" when "011010",
   "0101100110001100" when "011011",
   "0101100010010100" when "011100",
   "0101011110100001" when "011101",
   "0101011010110100" when "011110",
   "0101010111001011" when "011111",
   "0101010011101000" when "100000",
   "0101010000001001" when "100001",
   "0101001100101110" when "100010",
   "0101001001011000" when "100011",
   "0101000110000110" when "100100",
   "0101000010111000" when "100101",
   "0100111111101111" when "100110",
   "0100111100101001" when "100111",
   "0100111001100111" when "101000",
   "0100110110101001" when "101001",
   "0100110011101110" when "101010",
   "0100110000110110" when "101011",
   "0100101110000011" when "101100",
   "0100101011010010" when "101101",
   "0100101000100100" when "101110",
   "0100100101111010" when "101111",
   "0100100011010011" when "110000",
   "0100100000101111" when "110001",
   "0100011110001101" when "110010",
   "0100011011101110" when "110011",
   "0100011001010010" when "110100",
   "0100010110111001" when "110101",
   "0100010100100010" when "110110",
   "0100010010001110" when "110111",
   "0100001111111101" when "111000",
   "0100001101101101" when "111001",
   "0100001011100000" when "111010",
   "0100001001010110" when "111011",
   "0100000111001101" when "111100",
   "0100000101000111" when "111101",
   "0100000011000011" when "111110",
   "0100000001000001" when "111111",
   "----------------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                        GenericTable_6_10_F400_uid20
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity GenericTable_6_10_F400_uid20 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(9 downto 0)   );
end entity;

architecture arch of GenericTable_6_10_F400_uid20 is
signal TableOut :  std_logic_vector(9 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "0011010101" when "000000",
   "0010111000" when "000001",
   "0010011100" when "000010",
   "0010000000" when "000011",
   "0001100011" when "000100",
   "0001000111" when "000101",
   "0000101011" when "000110",
   "0000001110" when "000111",
   "0010101010" when "001000",
   "0010010100" when "001001",
   "0001111101" when "001010",
   "0001100110" when "001011",
   "0001001111" when "001100",
   "0000111001" when "001101",
   "0000100010" when "001110",
   "0000001011" when "001111",
   "0010001011" when "010000",
   "0001111001" when "010001",
   "0001100110" when "010010",
   "0001010100" when "010011",
   "0001000001" when "010100",
   "0000101110" when "010101",
   "0000011100" when "010110",
   "0000001001" when "010111",
   "0001110100" when "011000",
   "0001100101" when "011001",
   "0001010101" when "011010",
   "0001000110" when "011011",
   "0000110110" when "011100",
   "0000100111" when "011101",
   "0000010111" when "011110",
   "0000001000" when "011111",
   "0001100010" when "100000",
   "0001010101" when "100001",
   "0001001000" when "100010",
   "0000111011" when "100011",
   "0000101110" when "100100",
   "0000100001" when "100101",
   "0000010100" when "100110",
   "0000000111" when "100111",
   "0001010100" when "101000",
   "0001001001" when "101001",
   "0000111110" when "101010",
   "0000110011" when "101011",
   "0000100111" when "101100",
   "0000011100" when "101101",
   "0000010001" when "101110",
   "0000000110" when "101111",
   "0001001001" when "110000",
   "0000111111" when "110001",
   "0000110110" when "110010",
   "0000101100" when "110011",
   "0000100010" when "110100",
   "0000011000" when "110101",
   "0000001111" when "110110",
   "0000000101" when "110111",
   "0001000000" when "111000",
   "0000110111" when "111001",
   "0000101111" when "111010",
   "0000100110" when "111011",
   "0000011110" when "111100",
   "0000010101" when "111101",
   "0000001101" when "111110",
   "0000000100" when "111111",
   "----------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                              reciprocal_uid23
--        (BipartiteTable_f_2_1Px_M1bM12_in_M10_out_1_M12_F400_uid14)
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Matei Istoan (2014)
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
library work;

entity reciprocal_uid23 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(9 downto 0);
          Y : out  std_logic_vector(13 downto 0)   );
end entity;

architecture arch of reciprocal_uid23 is
   component GenericTable_6_16_F400_uid16 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(15 downto 0)   );
   end component;

   component GenericTable_6_10_F400_uid20 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(9 downto 0)   );
   end component;

signal X0 :  std_logic_vector(2 downto 0);
signal X1 :  std_logic_vector(2 downto 0);
signal X2 :  std_logic_vector(3 downto 0);
signal X2_msb :  std_logic;
signal X2_short :  std_logic_vector(2 downto 0);
signal X2_short_inv :  std_logic_vector(2 downto 0);
signal tableTIVaddr :  std_logic_vector(5 downto 0);
signal tableTOaddr :  std_logic_vector(5 downto 0);
signal tableTIVout :  std_logic_vector(15 downto 0);
signal tableTOout :  std_logic_vector(9 downto 0);
signal tableTOout_inv :  std_logic_vector(9 downto 0);
signal tableTIV_fxp :  signed(1+14 downto 0);
signal tableTO_fxp :  signed(-5+14 downto 0);
signal tableTO_fxp_sgnExt :  signed(1+14 downto 0);
signal Y_int :  signed(1+14 downto 0);
signal Y_int_short :  signed(1+13 downto 0);
signal Y_rnd :  signed(1+13 downto 0);
attribute rom_extract: string;
attribute rom_style: string;
attribute rom_extract of GenericTable_6_10_F400_uid20: component is "yes";
attribute rom_extract of GenericTable_6_16_F400_uid16: component is "yes";
attribute rom_style of GenericTable_6_10_F400_uid20: component is "block";
attribute rom_style of GenericTable_6_16_F400_uid16: component is "block";
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
   X0 <= X(9 downto 7);
   X1 <= X(6 downto 4);
   X2 <= X(3 downto 0);

   X2_msb <= X2(3);
   X2_short <= X2(2 downto 0);
   X2_short_inv <= X2_short xor (2 downto 0 => X2_msb);

   tableTIVaddr <= X0 & X1;
   tableTOaddr <= X0 & X2_short_inv;

   TIVtable: GenericTable_6_16_F400_uid16  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => tableTIVaddr,
                 Y => tableTIVout);

   TOtable: GenericTable_6_10_F400_uid20  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => tableTOaddr,
                 Y => tableTOout);

   tableTOout_inv <= tableTOout xor (9 downto 0 => X2_msb);

   tableTIV_fxp <= signed(tableTIVout);
   tableTO_fxp <= signed(tableTOout_inv);
   tableTO_fxp_sgnExt <= (5 downto 0 => tableTO_fxp(9)) & tableTO_fxp(9 downto 0); -- fix resize from (-5, -14) to (1, -14)

   Y_int <= tableTIV_fxp + tableTO_fxp_sgnExt;
   Y_int_short <= Y_int(15 downto 1); -- fix resize from (1, -14) to (1, -13)
   Y_rnd <= Y_int_short + ("00000000000000" & '1');
   Y <= std_logic_vector(Y_rnd(14 downto 1));
end architecture;

--------------------------------------------------------------------------------
--                        GenericTable_8_12_F400_uid29
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity GenericTable_8_12_F400_uid29 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(7 downto 0);
          Y : out  std_logic_vector(11 downto 0)   );
end entity;

architecture arch of GenericTable_8_12_F400_uid29 is
signal TableOut :  std_logic_vector(11 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000000101" when "00000000",
   "000000001111" when "00000001",
   "000000011001" when "00000010",
   "000000100011" when "00000011",
   "000000101110" when "00000100",
   "000000111000" when "00000101",
   "000001000010" when "00000110",
   "000001001100" when "00000111",
   "000001010110" when "00001000",
   "000001100000" when "00001001",
   "000001101011" when "00001010",
   "000001110101" when "00001011",
   "000001111111" when "00001100",
   "000010001001" when "00001101",
   "000010010011" when "00001110",
   "000010011101" when "00001111",
   "000010101000" when "00010000",
   "000010110010" when "00010001",
   "000010111100" when "00010010",
   "000011000110" when "00010011",
   "000011010000" when "00010100",
   "000011011010" when "00010101",
   "000011100100" when "00010110",
   "000011101110" when "00010111",
   "000011111000" when "00011000",
   "000100000011" when "00011001",
   "000100001101" when "00011010",
   "000100010111" when "00011011",
   "000100100001" when "00011100",
   "000100101011" when "00011101",
   "000100110101" when "00011110",
   "000100111111" when "00011111",
   "000101001001" when "00100000",
   "000101010011" when "00100001",
   "000101011101" when "00100010",
   "000101100111" when "00100011",
   "000101110001" when "00100100",
   "000101111011" when "00100101",
   "000110000101" when "00100110",
   "000110001111" when "00100111",
   "000110011001" when "00101000",
   "000110100011" when "00101001",
   "000110101101" when "00101010",
   "000110110111" when "00101011",
   "000111000000" when "00101100",
   "000111001010" when "00101101",
   "000111010100" when "00101110",
   "000111011110" when "00101111",
   "000111101000" when "00110000",
   "000111110010" when "00110001",
   "000111111100" when "00110010",
   "001000000101" when "00110011",
   "001000001111" when "00110100",
   "001000011001" when "00110101",
   "001000100011" when "00110110",
   "001000101100" when "00110111",
   "001000110110" when "00111000",
   "001001000000" when "00111001",
   "001001001010" when "00111010",
   "001001010011" when "00111011",
   "001001011101" when "00111100",
   "001001100110" when "00111101",
   "001001110000" when "00111110",
   "001001111010" when "00111111",
   "001010000011" when "01000000",
   "001010001101" when "01000001",
   "001010010110" when "01000010",
   "001010100000" when "01000011",
   "001010101001" when "01000100",
   "001010110011" when "01000101",
   "001010111100" when "01000110",
   "001011000110" when "01000111",
   "001011001111" when "01001000",
   "001011011001" when "01001001",
   "001011100010" when "01001010",
   "001011101100" when "01001011",
   "001011110101" when "01001100",
   "001011111110" when "01001101",
   "001100001000" when "01001110",
   "001100010001" when "01001111",
   "001100011010" when "01010000",
   "001100100011" when "01010001",
   "001100101101" when "01010010",
   "001100110110" when "01010011",
   "001100111111" when "01010100",
   "001101001000" when "01010101",
   "001101010001" when "01010110",
   "001101011011" when "01010111",
   "001101100100" when "01011000",
   "001101101101" when "01011001",
   "001101110110" when "01011010",
   "001101111111" when "01011011",
   "001110001000" when "01011100",
   "001110010001" when "01011101",
   "001110011010" when "01011110",
   "001110100011" when "01011111",
   "001110101100" when "01100000",
   "001110110101" when "01100001",
   "001110111110" when "01100010",
   "001111000110" when "01100011",
   "001111001111" when "01100100",
   "001111011000" when "01100101",
   "001111100001" when "01100110",
   "001111101010" when "01100111",
   "001111110010" when "01101000",
   "001111111011" when "01101001",
   "010000000100" when "01101010",
   "010000001100" when "01101011",
   "010000010101" when "01101100",
   "010000011110" when "01101101",
   "010000100110" when "01101110",
   "010000101111" when "01101111",
   "010000110111" when "01110000",
   "010001000000" when "01110001",
   "010001001000" when "01110010",
   "010001010001" when "01110011",
   "010001011001" when "01110100",
   "010001100010" when "01110101",
   "010001101010" when "01110110",
   "010001110011" when "01110111",
   "010001111011" when "01111000",
   "010010000011" when "01111001",
   "010010001100" when "01111010",
   "010010010100" when "01111011",
   "010010011100" when "01111100",
   "010010100100" when "01111101",
   "010010101100" when "01111110",
   "010010110101" when "01111111",
   "010010111101" when "10000000",
   "010011000101" when "10000001",
   "010011001101" when "10000010",
   "010011010101" when "10000011",
   "010011011101" when "10000100",
   "010011100101" when "10000101",
   "010011101101" when "10000110",
   "010011110101" when "10000111",
   "010011111101" when "10001000",
   "010100000101" when "10001001",
   "010100001101" when "10001010",
   "010100010101" when "10001011",
   "010100011101" when "10001100",
   "010100100100" when "10001101",
   "010100101100" when "10001110",
   "010100110100" when "10001111",
   "010100111100" when "10010000",
   "010101000011" when "10010001",
   "010101001011" when "10010010",
   "010101010011" when "10010011",
   "010101011010" when "10010100",
   "010101100010" when "10010101",
   "010101101010" when "10010110",
   "010101110001" when "10010111",
   "010101111001" when "10011000",
   "010110000000" when "10011001",
   "010110001000" when "10011010",
   "010110001111" when "10011011",
   "010110010111" when "10011100",
   "010110011110" when "10011101",
   "010110100101" when "10011110",
   "010110101101" when "10011111",
   "010110110100" when "10100000",
   "010110111011" when "10100001",
   "010111000011" when "10100010",
   "010111001010" when "10100011",
   "010111010001" when "10100100",
   "010111011000" when "10100101",
   "010111011111" when "10100110",
   "010111100111" when "10100111",
   "010111101110" when "10101000",
   "010111110101" when "10101001",
   "010111111100" when "10101010",
   "011000000011" when "10101011",
   "011000001010" when "10101100",
   "011000010001" when "10101101",
   "011000011000" when "10101110",
   "011000011111" when "10101111",
   "011000100110" when "10110000",
   "011000101101" when "10110001",
   "011000110100" when "10110010",
   "011000111010" when "10110011",
   "011001000001" when "10110100",
   "011001001000" when "10110101",
   "011001001111" when "10110110",
   "011001010101" when "10110111",
   "011001011100" when "10111000",
   "011001100011" when "10111001",
   "011001101010" when "10111010",
   "011001110000" when "10111011",
   "011001110111" when "10111100",
   "011001111101" when "10111101",
   "011010000100" when "10111110",
   "011010001011" when "10111111",
   "011010010001" when "11000000",
   "011010011000" when "11000001",
   "011010011110" when "11000010",
   "011010100100" when "11000011",
   "011010101011" when "11000100",
   "011010110001" when "11000101",
   "011010111000" when "11000110",
   "011010111110" when "11000111",
   "011011000100" when "11001000",
   "011011001011" when "11001001",
   "011011010001" when "11001010",
   "011011010111" when "11001011",
   "011011011101" when "11001100",
   "011011100100" when "11001101",
   "011011101010" when "11001110",
   "011011110000" when "11001111",
   "011011110110" when "11010000",
   "011011111100" when "11010001",
   "011100000010" when "11010010",
   "011100001000" when "11010011",
   "011100001110" when "11010100",
   "011100010100" when "11010101",
   "011100011010" when "11010110",
   "011100100000" when "11010111",
   "011100100110" when "11011000",
   "011100101100" when "11011001",
   "011100110010" when "11011010",
   "011100111000" when "11011011",
   "011100111110" when "11011100",
   "011101000100" when "11011101",
   "011101001010" when "11011110",
   "011101001111" when "11011111",
   "011101010101" when "11100000",
   "011101011011" when "11100001",
   "011101100001" when "11100010",
   "011101100110" when "11100011",
   "011101101100" when "11100100",
   "011101110010" when "11100101",
   "011101110111" when "11100110",
   "011101111101" when "11100111",
   "011110000010" when "11101000",
   "011110001000" when "11101001",
   "011110001110" when "11101010",
   "011110010011" when "11101011",
   "011110011001" when "11101100",
   "011110011110" when "11101101",
   "011110100100" when "11101110",
   "011110101001" when "11101111",
   "011110101110" when "11110000",
   "011110110100" when "11110001",
   "011110111001" when "11110010",
   "011110111111" when "11110011",
   "011111000100" when "11110100",
   "011111001001" when "11110101",
   "011111001111" when "11110110",
   "011111010100" when "11110111",
   "011111011001" when "11111000",
   "011111011110" when "11111001",
   "011111100100" when "11111010",
   "011111101001" when "11111011",
   "011111101110" when "11111100",
   "011111110011" when "11111101",
   "011111111000" when "11111110",
   "011111111101" when "11111111",
   "------------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                        GenericTable_7_4_F400_uid33
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity GenericTable_7_4_F400_uid33 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(6 downto 0);
          Y : out  std_logic_vector(3 downto 0)   );
end entity;

architecture arch of GenericTable_7_4_F400_uid33 is
signal TableOut :  std_logic_vector(3 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "1011" when "0000000",
   "1100" when "0000001",
   "1101" when "0000010",
   "1101" when "0000011",
   "1110" when "0000100",
   "1110" when "0000101",
   "1111" when "0000110",
   "1111" when "0000111",
   "1011" when "0001000",
   "1100" when "0001001",
   "1101" when "0001010",
   "1101" when "0001011",
   "1110" when "0001100",
   "1110" when "0001101",
   "1111" when "0001110",
   "1111" when "0001111",
   "1011" when "0010000",
   "1100" when "0010001",
   "1101" when "0010010",
   "1101" when "0010011",
   "1110" when "0010100",
   "1110" when "0010101",
   "1111" when "0010110",
   "1111" when "0010111",
   "1011" when "0011000",
   "1100" when "0011001",
   "1101" when "0011010",
   "1101" when "0011011",
   "1110" when "0011100",
   "1110" when "0011101",
   "1111" when "0011110",
   "1111" when "0011111",
   "1100" when "0100000",
   "1100" when "0100001",
   "1101" when "0100010",
   "1101" when "0100011",
   "1110" when "0100100",
   "1111" when "0100101",
   "1111" when "0100110",
   "1111" when "0100111",
   "1100" when "0101000",
   "1100" when "0101001",
   "1101" when "0101010",
   "1101" when "0101011",
   "1110" when "0101100",
   "1111" when "0101101",
   "1111" when "0101110",
   "1111" when "0101111",
   "1100" when "0110000",
   "1100" when "0110001",
   "1101" when "0110010",
   "1110" when "0110011",
   "1110" when "0110100",
   "1111" when "0110101",
   "1111" when "0110110",
   "1111" when "0110111",
   "1100" when "0111000",
   "1101" when "0111001",
   "1101" when "0111010",
   "1110" when "0111011",
   "1110" when "0111100",
   "1111" when "0111101",
   "1111" when "0111110",
   "1111" when "0111111",
   "1100" when "1000000",
   "1101" when "1000001",
   "1101" when "1000010",
   "1110" when "1000011",
   "1110" when "1000100",
   "1111" when "1000101",
   "1111" when "1000110",
   "1111" when "1000111",
   "1100" when "1001000",
   "1101" when "1001001",
   "1101" when "1001010",
   "1110" when "1001011",
   "1110" when "1001100",
   "1111" when "1001101",
   "1111" when "1001110",
   "1111" when "1001111",
   "1101" when "1010000",
   "1101" when "1010001",
   "1110" when "1010010",
   "1110" when "1010011",
   "1110" when "1010100",
   "1111" when "1010101",
   "1111" when "1010110",
   "1111" when "1010111",
   "1101" when "1011000",
   "1101" when "1011001",
   "1110" when "1011010",
   "1110" when "1011011",
   "1111" when "1011100",
   "1111" when "1011101",
   "1111" when "1011110",
   "1111" when "1011111",
   "1101" when "1100000",
   "1101" when "1100001",
   "1110" when "1100010",
   "1110" when "1100011",
   "1111" when "1100100",
   "1111" when "1100101",
   "1111" when "1100110",
   "1111" when "1100111",
   "1101" when "1101000",
   "1110" when "1101001",
   "1110" when "1101010",
   "1110" when "1101011",
   "1111" when "1101100",
   "1111" when "1101101",
   "1111" when "1101110",
   "1111" when "1101111",
   "1101" when "1110000",
   "1110" when "1110001",
   "1110" when "1110010",
   "1110" when "1110011",
   "1111" when "1110100",
   "1111" when "1110101",
   "1111" when "1110110",
   "1111" when "1110111",
   "1110" when "1111000",
   "1110" when "1111001",
   "1110" when "1111010",
   "1111" when "1111011",
   "1111" when "1111100",
   "1111" when "1111101",
   "1111" when "1111110",
   "1111" when "1111111",
   "----" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                                 atan_uid36
--         (BipartiteTable_f_atan_x_pi_in_M12_out_M2_M11_F400_uid27)
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Matei Istoan (2014)
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
library work;

entity atan_uid36 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(11 downto 0);
          Y : out  std_logic_vector(9 downto 0)   );
end entity;

architecture arch of atan_uid36 is
   component GenericTable_8_12_F400_uid29 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(7 downto 0);
             Y : out  std_logic_vector(11 downto 0)   );
   end component;

   component GenericTable_7_4_F400_uid33 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(6 downto 0);
             Y : out  std_logic_vector(3 downto 0)   );
   end component;

signal X0 :  std_logic_vector(3 downto 0);
signal X1 :  std_logic_vector(3 downto 0);
signal X2 :  std_logic_vector(3 downto 0);
signal X2_msb :  std_logic;
signal X2_short :  std_logic_vector(2 downto 0);
signal X2_short_inv :  std_logic_vector(2 downto 0);
signal tableTIVaddr :  std_logic_vector(7 downto 0);
signal tableTOaddr :  std_logic_vector(6 downto 0);
signal tableTIVout :  std_logic_vector(11 downto 0);
signal tableTOout :  std_logic_vector(3 downto 0);
signal tableTOout_inv :  std_logic_vector(3 downto 0);
signal tableTIV_fxp :  signed(-2+13 downto 0);
signal tableTO_fxp :  signed(-10+13 downto 0);
signal tableTO_fxp_sgnExt :  signed(-2+13 downto 0);
signal Y_int :  signed(-2+13 downto 0);
signal Y_int_short :  signed(-2+12 downto 0);
signal Y_rnd :  signed(-2+12 downto 0);
attribute rom_extract: string;
attribute rom_style: string;
attribute rom_extract of GenericTable_7_4_F400_uid33: component is "yes";
attribute rom_extract of GenericTable_8_12_F400_uid29: component is "yes";
attribute rom_style of GenericTable_7_4_F400_uid33: component is "block";
attribute rom_style of GenericTable_8_12_F400_uid29: component is "block";
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
   X0 <= X(11 downto 8);
   X1 <= X(7 downto 4);
   X2 <= X(3 downto 0);

   X2_msb <= X2(3);
   X2_short <= X2(2 downto 0);
   X2_short_inv <= X2_short xor (2 downto 0 => X2_msb);

   tableTIVaddr <= X0 & X1;
   tableTOaddr <= X0 & X2_short_inv;

   TIVtable: GenericTable_8_12_F400_uid29  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => tableTIVaddr,
                 Y => tableTIVout);

   TOtable: GenericTable_7_4_F400_uid33  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => tableTOaddr,
                 Y => tableTOout);

   tableTOout_inv <= tableTOout xor (3 downto 0 => X2_msb);

   tableTIV_fxp <= signed(tableTIVout);
   tableTO_fxp <= signed(tableTOout_inv);
   tableTO_fxp_sgnExt <= (7 downto 0 => tableTO_fxp(3)) & tableTO_fxp(3 downto 0); -- fix resize from (-10, -13) to (-2, -13)

   Y_int <= tableTIV_fxp + tableTO_fxp_sgnExt;
   Y_int_short <= Y_int(11 downto 1); -- fix resize from (-2, -13) to (-2, -12)
   Y_rnd <= Y_int_short + ("0000000000" & '1');
   Y <= std_logic_vector(Y_rnd(10 downto 1));
end architecture;

--------------------------------------------------------------------------------
--                  FixAtan2ByRecipMultAtan_12_12_F400_uid2
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Matei Istoan, Florent de Dinechin (2012-...)
--------------------------------------------------------------------------------
-- Pipeline depth: 4 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity FixAtan2ByRecipMultAtan_12_12_F400_uid2 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(11 downto 0);
          Y : in  std_logic_vector(11 downto 0);
          A : out  std_logic_vector(11 downto 0)   );
end entity;

architecture arch of FixAtan2ByRecipMultAtan_12_12_F400_uid2 is
   component LZOC_10_F400_uid4 is
      port ( clk, rst : in std_logic;
             I : in  std_logic_vector(9 downto 0);
             OZB : in  std_logic;
             O : out  std_logic_vector(3 downto 0)   );
   end component;

   component LeftShifter_11_by_max_10_F400_uid8 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(10 downto 0);
             S : in  std_logic_vector(3 downto 0);
             R : out  std_logic_vector(20 downto 0)   );
   end component;

   component reciprocal_uid23 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(9 downto 0);
             Y : out  std_logic_vector(13 downto 0)   );
   end component;

   component atan_uid36 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(11 downto 0);
             Y : out  std_logic_vector(9 downto 0)   );
   end component;

signal sgnX :  std_logic;
signal sgnY :  std_logic;
signal Xsat :  std_logic_vector(11 downto 0);
signal Ysat :  std_logic_vector(11 downto 0);
signal pX :  std_logic_vector(11 downto 0);
signal pY :  std_logic_vector(11 downto 0);
signal mX :  std_logic_vector(11 downto 0);
signal mY :  std_logic_vector(11 downto 0);
signal XmY :  std_logic_vector(12 downto 0);
signal XpY :  std_logic_vector(12 downto 0);
signal XltY :  std_logic;
signal mYltX :  std_logic;
signal quadrant, quadrant_d1, quadrant_d2, quadrant_d3, quadrant_d4 :  std_logic_vector(1 downto 0);
signal XR, XR_d1, XR_d2 :  std_logic_vector(10 downto 0);
signal YR, YR_d1, YR_d2 :  std_logic_vector(10 downto 0);
signal finalAdd, finalAdd_d1, finalAdd_d2, finalAdd_d3, finalAdd_d4 :  std_logic;
signal XorY :  std_logic_vector(9 downto 0);
signal S :  std_logic_vector(3 downto 0);
signal XRSfull :  std_logic_vector(20 downto 0);
signal XRS, XRS_d1 :  std_logic_vector(10 downto 0);
signal YRSfull :  std_logic_vector(20 downto 0);
signal YRS, YRS_d1 :  std_logic_vector(10 downto 0);
signal XRm1 :  std_logic_vector(9 downto 0);
signal R0 :  std_logic_vector(13 downto 0);
signal R, R_d1 :  unsigned(0+12 downto 0);
signal YRU, YRU_d1 :  unsigned(-1+11 downto 0);
signal P :  unsigned(0+23 downto 0);
signal PtruncU :  unsigned(-1+12 downto 0);
signal P_slv :  std_logic_vector(11 downto 0);
signal atanTableOut :  std_logic_vector(9 downto 0);
signal finalZ :  std_logic_vector(11 downto 0);
signal qangle :  std_logic_vector(11 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            quadrant_d1 <=  quadrant;
            quadrant_d2 <=  quadrant_d1;
            quadrant_d3 <=  quadrant_d2;
            quadrant_d4 <=  quadrant_d3;
            XR_d1 <=  XR;
            XR_d2 <=  XR_d1;
            YR_d1 <=  YR;
            YR_d2 <=  YR_d1;
            finalAdd_d1 <=  finalAdd;
            finalAdd_d2 <=  finalAdd_d1;
            finalAdd_d3 <=  finalAdd_d2;
            finalAdd_d4 <=  finalAdd_d3;
            XRS_d1 <=  XRS;
            YRS_d1 <=  YRS;
            R_d1 <=  R;
            YRU_d1 <=  YRU;
         end if;
      end process;
   sgnX <= X(11);
   sgnY <= Y(11);
   -- First saturate x and y in case they touch -1
   Xsat <= "100000000001" when X="100000000000" else X ;
   Ysat <= "100000000001" when Y="100000000000" else Y ;
   pX <= Xsat;
   pY <= Ysat;
   mX <= ("000000000000" - Xsat);
   mY <= ("000000000000" - Ysat);
   XmY <= (sgnX & Xsat)-(sgnY & Ysat);
   XpY <= (sgnX & Xsat)+(sgnY & Ysat);
   XltY <= XmY(12);
   mYltX <= not XpY(12);
   -- quadrant will also be the angle to add at the end
   quadrant <= 
      "00"  when (not sgnX and not XltY and     mYltX)='1' else
      "01"  when (not sgnY and     XltY and     mYltX)='1' else
      "10"  when (    sgnX and     XltY and not mYltX)='1' else
      "11";
   XR <= 
      pX(10 downto 0) when quadrant="00"   else 
      pY(10 downto 0) when quadrant="01"   else 
      mX(10 downto 0) when quadrant="10"   else 
      mY(10 downto 0);
   YR <= 
      pY(10 downto 0) when quadrant="00" and sgnY='0'  else 
      mY(10 downto 0) when quadrant="00" and sgnY='1'  else 
      pX(10 downto 0) when quadrant="01" and sgnX='0'  else 
      mX(10 downto 0) when quadrant="01" and sgnX='1'  else 
      pY(10 downto 0) when quadrant="10" and sgnY='0'  else 
      mY(10 downto 0) when quadrant="10" and sgnY='1'  else 
      pX(10 downto 0) when quadrant="11" and sgnX='0'  else 
      mX(10 downto 0) ;
   finalAdd <= 
      '1' when (quadrant="00" and sgnY='0') or(quadrant="01" and sgnX='1') or (quadrant="10" and sgnY='1') or (quadrant="11" and sgnX='0')
       else '0';  -- this information is sent to the end of the pipeline, better compute it here as one bit
   XorY <= XR(10 downto 1) or YR(10 downto 1);
   lzc: LZOC_10_F400_uid4  -- pipelineDepth=2 maxInDelay=2.00544e-09
      port map ( clk  => clk,
                 rst  => rst,
                 I => XorY,
                 O => S,
                 OZB => '0');
   ----------------Synchro barrier, entering cycle 2----------------
   Xshift: LeftShifter_11_by_max_10_F400_uid8  -- pipelineDepth=1 maxInDelay=1.07033e-09
      port map ( clk  => clk,
                 rst  => rst,
                 R => XRSfull,
                 S => S,
                 X => XR_d2);
   XRS <=  XRSfull (10 downto 0);
   Yshift: LeftShifter_11_by_max_10_F400_uid8  -- pipelineDepth=1 maxInDelay=1.07033e-09
      port map ( clk  => clk,
                 rst  => rst,
                 R => YRSfull,
                 S => S,
                 X => YR_d2);
   YRS <=  YRSfull (10 downto 0);
   ----------------Synchro barrier, entering cycle 3----------------
   XRm1 <= XRS_d1(9 downto 0); -- removing the MSB which is constantly 1
   recipTable: reciprocal_uid23  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => XRm1,
                 Y => R0);
   R <= unsigned(R0(12 downto 0)); -- removing the sign  bit
   YRU <= unsigned(YRS_d1);
   ----------------Synchro barrier, entering cycle 4----------------
   P <= R_d1*YRU_d1;
   PtruncU <= P(22 downto 11); -- fix resize from (0, -23) to (-1, -12)
   P_slv <=  std_logic_vector(PtruncU);
   atanTable: atan_uid36  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => P_slv,
                 Y => atanTableOut);
   finalZ <= "00" & atanTableOut;
   qangle <= (quadrant_d4 & "0000000000");
   A <=            qangle + finalZ  when finalAdd_d4='1'
      else qangle - finalZ;
end architecture;

