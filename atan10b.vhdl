--------------------------------------------------------------------------------
--                              LZOC_8_F400_uid4
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

entity LZOC_8_F400_uid4 is
   port ( clk, rst : in std_logic;
          I : in  std_logic_vector(7 downto 0);
          OZB : in  std_logic;
          O : out  std_logic_vector(3 downto 0)   );
end entity;

architecture arch of LZOC_8_F400_uid4 is
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
   level4<= I& (7 downto 0 => not(sozb));
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
--                      LeftShifter_9_by_max_8_F400_uid8
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

entity LeftShifter_9_by_max_8_F400_uid8 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(8 downto 0);
          S : in  std_logic_vector(3 downto 0);
          R : out  std_logic_vector(16 downto 0)   );
end entity;

architecture arch of LeftShifter_9_by_max_8_F400_uid8 is
signal level0 :  std_logic_vector(8 downto 0);
signal ps, ps_d1 :  std_logic_vector(3 downto 0);
signal level1 :  std_logic_vector(9 downto 0);
signal level2 :  std_logic_vector(11 downto 0);
signal level3, level3_d1 :  std_logic_vector(15 downto 0);
signal level4 :  std_logic_vector(23 downto 0);
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
   R <= level4(16 downto 0);
end architecture;

--------------------------------------------------------------------------------
--                        GenericTable_4_14_F400_uid16
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity GenericTable_4_14_F400_uid16 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(3 downto 0);
          Y : out  std_logic_vector(13 downto 0)   );
end entity;

architecture arch of GenericTable_4_14_F400_uid16 is
signal TableOut :  std_logic_vector(13 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "01111100011001" when "0000",
   "01110101010001" when "0001",
   "01101110111010" when "0010",
   "01101001001100" when "0011",
   "01100100000011" when "0100",
   "01011111011000" when "0101",
   "01011011001000" when "0110",
   "01010111001111" when "0111",
   "01010011101011" when "1000",
   "01010000011000" when "1001",
   "01001101010110" when "1010",
   "01001010100001" when "1011",
   "01000111111001" when "1100",
   "01000101011101" when "1101",
   "01000011001011" when "1110",
   "01000001000010" when "1111",
   "--------------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                        GenericTable_5_10_F400_uid20
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity GenericTable_5_10_F400_uid20 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(9 downto 0)   );
end entity;

architecture arch of GenericTable_5_10_F400_uid20 is
signal TableOut :  std_logic_vector(9 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "0010111110" when "00000",
   "0010100101" when "00001",
   "0010001100" when "00010",
   "0001110010" when "00011",
   "0001011001" when "00100",
   "0000111111" when "00101",
   "0000100110" when "00110",
   "0000001101" when "00111",
   "0001111111" when "01000",
   "0001101110" when "01001",
   "0001011101" when "01010",
   "0001001100" when "01011",
   "0000111011" when "01100",
   "0000101010" when "01101",
   "0000011001" when "01110",
   "0000001000" when "01111",
   "0001011011" when "10000",
   "0001001111" when "10001",
   "0001000011" when "10010",
   "0000110111" when "10011",
   "0000101011" when "10100",
   "0000011110" when "10101",
   "0000010010" when "10110",
   "0000000110" when "10111",
   "0001000100" when "11000",
   "0000111011" when "11001",
   "0000110010" when "11010",
   "0000101001" when "11011",
   "0000100000" when "11100",
   "0000010111" when "11101",
   "0000001110" when "11110",
   "0000000101" when "11111",
   "----------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                              reciprocal_uid23
--         (BipartiteTable_f_2_1Px_M1bM10_in_M8_out_1_M10_F400_uid14)
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
          X : in  std_logic_vector(7 downto 0);
          Y : out  std_logic_vector(11 downto 0)   );
end entity;

architecture arch of reciprocal_uid23 is
   component GenericTable_4_14_F400_uid16 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(3 downto 0);
             Y : out  std_logic_vector(13 downto 0)   );
   end component;

   component GenericTable_5_10_F400_uid20 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(9 downto 0)   );
   end component;

signal X0 :  std_logic_vector(1 downto 0);
signal X1 :  std_logic_vector(1 downto 0);
signal X2 :  std_logic_vector(3 downto 0);
signal X2_msb :  std_logic;
signal X2_short :  std_logic_vector(2 downto 0);
signal X2_short_inv :  std_logic_vector(2 downto 0);
signal tableTIVaddr :  std_logic_vector(3 downto 0);
signal tableTOaddr :  std_logic_vector(4 downto 0);
signal tableTIVout :  std_logic_vector(13 downto 0);
signal tableTOout :  std_logic_vector(9 downto 0);
signal tableTOout_inv :  std_logic_vector(9 downto 0);
signal tableTIV_fxp :  signed(1+12 downto 0);
signal tableTO_fxp :  signed(-3+12 downto 0);
signal tableTO_fxp_sgnExt :  signed(1+12 downto 0);
signal Y_int :  signed(1+12 downto 0);
signal Y_int_short :  signed(1+11 downto 0);
signal Y_rnd :  signed(1+11 downto 0);
attribute rom_extract: string;
attribute rom_style: string;
attribute rom_extract of GenericTable_4_14_F400_uid16: component is "yes";
attribute rom_extract of GenericTable_5_10_F400_uid20: component is "yes";
attribute rom_style of GenericTable_4_14_F400_uid16: component is "block";
attribute rom_style of GenericTable_5_10_F400_uid20: component is "block";
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
   X0 <= X(7 downto 6);
   X1 <= X(5 downto 4);
   X2 <= X(3 downto 0);

   X2_msb <= X2(3);
   X2_short <= X2(2 downto 0);
   X2_short_inv <= X2_short xor (2 downto 0 => X2_msb);

   tableTIVaddr <= X0 & X1;
   tableTOaddr <= X0 & X2_short_inv;

   TIVtable: GenericTable_4_14_F400_uid16  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => tableTIVaddr,
                 Y => tableTIVout);

   TOtable: GenericTable_5_10_F400_uid20  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => tableTOaddr,
                 Y => tableTOout);

   tableTOout_inv <= tableTOout xor (9 downto 0 => X2_msb);

   tableTIV_fxp <= signed(tableTIVout);
   tableTO_fxp <= signed(tableTOout_inv);
   tableTO_fxp_sgnExt <= (3 downto 0 => tableTO_fxp(9)) & tableTO_fxp(9 downto 0); -- fix resize from (-3, -12) to (1, -12)

   Y_int <= tableTIV_fxp + tableTO_fxp_sgnExt;
   Y_int_short <= Y_int(13 downto 1); -- fix resize from (1, -12) to (1, -11)
   Y_rnd <= Y_int_short + ("000000000000" & '1');
   Y <= std_logic_vector(Y_rnd(12 downto 1));
end architecture;

--------------------------------------------------------------------------------
--                        GenericTable_6_10_F400_uid29
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity GenericTable_6_10_F400_uid29 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(9 downto 0)   );
end entity;

architecture arch of GenericTable_6_10_F400_uid29 is
signal TableOut :  std_logic_vector(9 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "0000000101" when "000000",
   "0000001111" when "000001",
   "0000011001" when "000010",
   "0000100011" when "000011",
   "0000101101" when "000100",
   "0000111000" when "000101",
   "0001000010" when "000110",
   "0001001100" when "000111",
   "0001010110" when "001000",
   "0001100000" when "001001",
   "0001101010" when "001010",
   "0001110100" when "001011",
   "0001111101" when "001100",
   "0010000111" when "001101",
   "0010010001" when "001110",
   "0010011011" when "001111",
   "0010100100" when "010000",
   "0010101110" when "010001",
   "0010110111" when "010010",
   "0011000000" when "010011",
   "0011001010" when "010100",
   "0011010011" when "010101",
   "0011011100" when "010110",
   "0011100101" when "010111",
   "0011101110" when "011000",
   "0011110111" when "011001",
   "0100000000" when "011010",
   "0100001000" when "011011",
   "0100010001" when "011100",
   "0100011001" when "011101",
   "0100100010" when "011110",
   "0100101010" when "011111",
   "0100110010" when "100000",
   "0100111010" when "100001",
   "0101000010" when "100010",
   "0101001010" when "100011",
   "0101010010" when "100100",
   "0101011001" when "100101",
   "0101100001" when "100110",
   "0101101000" when "100111",
   "0101110000" when "101000",
   "0101110111" when "101001",
   "0101111110" when "101010",
   "0110000101" when "101011",
   "0110001100" when "101100",
   "0110010011" when "101101",
   "0110011001" when "101110",
   "0110100000" when "101111",
   "0110100111" when "110000",
   "0110101101" when "110001",
   "0110110011" when "110010",
   "0110111010" when "110011",
   "0111000000" when "110100",
   "0111000110" when "110101",
   "0111001100" when "110110",
   "0111010010" when "110111",
   "0111010111" when "111000",
   "0111011101" when "111001",
   "0111100011" when "111010",
   "0111101000" when "111011",
   "0111101110" when "111100",
   "0111110011" when "111101",
   "0111111000" when "111110",
   "0111111101" when "111111",
   "----------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                        GenericTable_6_4_F400_uid33
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity GenericTable_6_4_F400_uid33 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(3 downto 0)   );
end entity;

architecture arch of GenericTable_6_4_F400_uid33 is
signal TableOut :  std_logic_vector(3 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "1011" when "000000",
   "1100" when "000001",
   "1101" when "000010",
   "1101" when "000011",
   "1110" when "000100",
   "1110" when "000101",
   "1111" when "000110",
   "1111" when "000111",
   "1011" when "001000",
   "1100" when "001001",
   "1101" when "001010",
   "1101" when "001011",
   "1110" when "001100",
   "1110" when "001101",
   "1111" when "001110",
   "1111" when "001111",
   "1100" when "010000",
   "1100" when "010001",
   "1101" when "010010",
   "1101" when "010011",
   "1110" when "010100",
   "1111" when "010101",
   "1111" when "010110",
   "1111" when "010111",
   "1100" when "011000",
   "1101" when "011001",
   "1101" when "011010",
   "1110" when "011011",
   "1110" when "011100",
   "1111" when "011101",
   "1111" when "011110",
   "1111" when "011111",
   "1100" when "100000",
   "1101" when "100001",
   "1101" when "100010",
   "1110" when "100011",
   "1110" when "100100",
   "1111" when "100101",
   "1111" when "100110",
   "1111" when "100111",
   "1101" when "101000",
   "1101" when "101001",
   "1110" when "101010",
   "1110" when "101011",
   "1110" when "101100",
   "1111" when "101101",
   "1111" when "101110",
   "1111" when "101111",
   "1101" when "110000",
   "1110" when "110001",
   "1110" when "110010",
   "1110" when "110011",
   "1111" when "110100",
   "1111" when "110101",
   "1111" when "110110",
   "1111" when "110111",
   "1101" when "111000",
   "1110" when "111001",
   "1110" when "111010",
   "1110" when "111011",
   "1111" when "111100",
   "1111" when "111101",
   "1111" when "111110",
   "1111" when "111111",
   "----" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                                 atan_uid36
--          (BipartiteTable_f_atan_x_pi_in_M10_out_M2_M9_F400_uid27)
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
          X : in  std_logic_vector(9 downto 0);
          Y : out  std_logic_vector(7 downto 0)   );
end entity;

architecture arch of atan_uid36 is
   component GenericTable_6_10_F400_uid29 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(9 downto 0)   );
   end component;

   component GenericTable_6_4_F400_uid33 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(3 downto 0)   );
   end component;

signal X0 :  std_logic_vector(2 downto 0);
signal X1 :  std_logic_vector(2 downto 0);
signal X2 :  std_logic_vector(3 downto 0);
signal X2_msb :  std_logic;
signal X2_short :  std_logic_vector(2 downto 0);
signal X2_short_inv :  std_logic_vector(2 downto 0);
signal tableTIVaddr :  std_logic_vector(5 downto 0);
signal tableTOaddr :  std_logic_vector(5 downto 0);
signal tableTIVout :  std_logic_vector(9 downto 0);
signal tableTOout :  std_logic_vector(3 downto 0);
signal tableTOout_inv :  std_logic_vector(3 downto 0);
signal tableTIV_fxp :  signed(-2+11 downto 0);
signal tableTO_fxp :  signed(-8+11 downto 0);
signal tableTO_fxp_sgnExt :  signed(-2+11 downto 0);
signal Y_int :  signed(-2+11 downto 0);
signal Y_int_short :  signed(-2+10 downto 0);
signal Y_rnd :  signed(-2+10 downto 0);
attribute rom_extract: string;
attribute rom_style: string;
attribute rom_extract of GenericTable_6_10_F400_uid29: component is "yes";
attribute rom_extract of GenericTable_6_4_F400_uid33: component is "yes";
attribute rom_style of GenericTable_6_10_F400_uid29: component is "block";
attribute rom_style of GenericTable_6_4_F400_uid33: component is "block";
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

   TIVtable: GenericTable_6_10_F400_uid29  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => tableTIVaddr,
                 Y => tableTIVout);

   TOtable: GenericTable_6_4_F400_uid33  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => tableTOaddr,
                 Y => tableTOout);

   tableTOout_inv <= tableTOout xor (3 downto 0 => X2_msb);

   tableTIV_fxp <= signed(tableTIVout);
   tableTO_fxp <= signed(tableTOout_inv);
   tableTO_fxp_sgnExt <= (5 downto 0 => tableTO_fxp(3)) & tableTO_fxp(3 downto 0); -- fix resize from (-8, -11) to (-2, -11)

   Y_int <= tableTIV_fxp + tableTO_fxp_sgnExt;
   Y_int_short <= Y_int(9 downto 1); -- fix resize from (-2, -11) to (-2, -10)
   Y_rnd <= Y_int_short + ("00000000" & '1');
   Y <= std_logic_vector(Y_rnd(8 downto 1));
end architecture;

--------------------------------------------------------------------------------
--                  FixAtan2ByRecipMultAtan_10_10_F400_uid2
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

entity FixAtan2ByRecipMultAtan_10_10_F400_uid2 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(9 downto 0);
          Y : in  std_logic_vector(9 downto 0);
          A : out  std_logic_vector(9 downto 0)   );
end entity;

architecture arch of FixAtan2ByRecipMultAtan_10_10_F400_uid2 is
   component LZOC_8_F400_uid4 is
      port ( clk, rst : in std_logic;
             I : in  std_logic_vector(7 downto 0);
             OZB : in  std_logic;
             O : out  std_logic_vector(3 downto 0)   );
   end component;

   component LeftShifter_9_by_max_8_F400_uid8 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(8 downto 0);
             S : in  std_logic_vector(3 downto 0);
             R : out  std_logic_vector(16 downto 0)   );
   end component;

   component reciprocal_uid23 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(7 downto 0);
             Y : out  std_logic_vector(11 downto 0)   );
   end component;

   component atan_uid36 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(9 downto 0);
             Y : out  std_logic_vector(7 downto 0)   );
   end component;

signal sgnX :  std_logic;
signal sgnY :  std_logic;
signal Xsat :  std_logic_vector(9 downto 0);
signal Ysat :  std_logic_vector(9 downto 0);
signal pX :  std_logic_vector(9 downto 0);
signal pY :  std_logic_vector(9 downto 0);
signal mX :  std_logic_vector(9 downto 0);
signal mY :  std_logic_vector(9 downto 0);
signal XmY :  std_logic_vector(10 downto 0);
signal XpY :  std_logic_vector(10 downto 0);
signal XltY :  std_logic;
signal mYltX :  std_logic;
signal quadrant, quadrant_d1, quadrant_d2, quadrant_d3, quadrant_d4 :  std_logic_vector(1 downto 0);
signal XR, XR_d1, XR_d2 :  std_logic_vector(8 downto 0);
signal YR, YR_d1, YR_d2 :  std_logic_vector(8 downto 0);
signal finalAdd, finalAdd_d1, finalAdd_d2, finalAdd_d3, finalAdd_d4 :  std_logic;
signal XorY :  std_logic_vector(7 downto 0);
signal S :  std_logic_vector(3 downto 0);
signal XRSfull :  std_logic_vector(16 downto 0);
signal XRS, XRS_d1 :  std_logic_vector(8 downto 0);
signal YRSfull :  std_logic_vector(16 downto 0);
signal YRS, YRS_d1 :  std_logic_vector(8 downto 0);
signal XRm1 :  std_logic_vector(7 downto 0);
signal R0 :  std_logic_vector(11 downto 0);
signal R, R_d1 :  unsigned(0+10 downto 0);
signal YRU, YRU_d1 :  unsigned(-1+9 downto 0);
signal P :  unsigned(0+19 downto 0);
signal PtruncU :  unsigned(-1+10 downto 0);
signal P_slv :  std_logic_vector(9 downto 0);
signal atanTableOut :  std_logic_vector(7 downto 0);
signal finalZ :  std_logic_vector(9 downto 0);
signal qangle :  std_logic_vector(9 downto 0);
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
   sgnX <= X(9);
   sgnY <= Y(9);
   -- First saturate x and y in case they touch -1
   Xsat <= "1000000001" when X="1000000000" else X ;
   Ysat <= "1000000001" when Y="1000000000" else Y ;
   pX <= Xsat;
   pY <= Ysat;
   mX <= ("0000000000" - Xsat);
   mY <= ("0000000000" - Ysat);
   XmY <= (sgnX & Xsat)-(sgnY & Ysat);
   XpY <= (sgnX & Xsat)+(sgnY & Ysat);
   XltY <= XmY(10);
   mYltX <= not XpY(10);
   -- quadrant will also be the angle to add at the end
   quadrant <= 
      "00"  when (not sgnX and not XltY and     mYltX)='1' else
      "01"  when (not sgnY and     XltY and     mYltX)='1' else
      "10"  when (    sgnX and     XltY and not mYltX)='1' else
      "11";
   XR <= 
      pX(8 downto 0) when quadrant="00"   else 
      pY(8 downto 0) when quadrant="01"   else 
      mX(8 downto 0) when quadrant="10"   else 
      mY(8 downto 0);
   YR <= 
      pY(8 downto 0) when quadrant="00" and sgnY='0'  else 
      mY(8 downto 0) when quadrant="00" and sgnY='1'  else 
      pX(8 downto 0) when quadrant="01" and sgnX='0'  else 
      mX(8 downto 0) when quadrant="01" and sgnX='1'  else 
      pY(8 downto 0) when quadrant="10" and sgnY='0'  else 
      mY(8 downto 0) when quadrant="10" and sgnY='1'  else 
      pX(8 downto 0) when quadrant="11" and sgnX='0'  else 
      mX(8 downto 0) ;
   finalAdd <= 
      '1' when (quadrant="00" and sgnY='0') or(quadrant="01" and sgnX='1') or (quadrant="10" and sgnY='1') or (quadrant="11" and sgnX='0')
       else '0';  -- this information is sent to the end of the pipeline, better compute it here as one bit
   XorY <= XR(8 downto 1) or YR(8 downto 1);
   lzc: LZOC_8_F400_uid4  -- pipelineDepth=2 maxInDelay=1.95944e-09
      port map ( clk  => clk,
                 rst  => rst,
                 I => XorY,
                 O => S,
                 OZB => '0');
   ----------------Synchro barrier, entering cycle 2----------------
   Xshift: LeftShifter_9_by_max_8_F400_uid8  -- pipelineDepth=1 maxInDelay=1.07033e-09
      port map ( clk  => clk,
                 rst  => rst,
                 R => XRSfull,
                 S => S,
                 X => XR_d2);
   XRS <=  XRSfull (8 downto 0);
   Yshift: LeftShifter_9_by_max_8_F400_uid8  -- pipelineDepth=1 maxInDelay=1.07033e-09
      port map ( clk  => clk,
                 rst  => rst,
                 R => YRSfull,
                 S => S,
                 X => YR_d2);
   YRS <=  YRSfull (8 downto 0);
   ----------------Synchro barrier, entering cycle 3----------------
   XRm1 <= XRS_d1(7 downto 0); -- removing the MSB which is constantly 1
   recipTable: reciprocal_uid23  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => XRm1,
                 Y => R0);
   R <= unsigned(R0(10 downto 0)); -- removing the sign  bit
   YRU <= unsigned(YRS_d1);
   ----------------Synchro barrier, entering cycle 4----------------
   P <= R_d1*YRU_d1;
   PtruncU <= P(18 downto 9); -- fix resize from (0, -19) to (-1, -10)
   P_slv <=  std_logic_vector(PtruncU);
   atanTable: atan_uid36  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => P_slv,
                 Y => atanTableOut);
   finalZ <= "00" & atanTableOut;
   qangle <= (quadrant_d4 & "00000000");
   A <=            qangle + finalZ  when finalAdd_d4='1'
      else qangle - finalZ;
end architecture;

