----------------------------------------------------------------------------------
-- DDS Sinc Generator
----------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.all;

entity DDS_SINC_GENERATOR is
	generic (
	   out_WIDTH : positive := 16;   -- output sinusoid width (IP CORE fixed at 16 bits)
	   pacc_WIDTH : positive := 16;  -- phase accumulator width (IP CORE fixed at 16 bits) 
	   cnt_WIDTH : positive;         -- phase accumulator counter width
	   pinc_WIDTH : positive         -- phase increment width -> same as counter
	);
	 port(
		 clk : in std_logic;	
		 reset : in std_logic;					                
		 freq_value : in std_logic_vector(cnt_WIDTH - 1 downto 0);    -- Selects output frequency (phase increment)
		 phase_offset : in std_logic_vector(cnt_width - 1 downto 0);  -- Selects phase offset from 0 degrees
		 load_freq : in std_logic;                                    -- Pulse to load new frequency (or phase offset)
		 phase_out : out std_logic_vector(pacc_WIDTH - 1 downto 0);   -- Internal phase accumulator output
		 phase_count : out std_logic_vector(cnt_WIDTH -1 downto 0);   -- Internal phase accumulator counter output
		 sinc_out : out std_logic_vector(out_WIDTH - 1 downto 0);     -- Output sine signal
		 );
end DDS_SINC_GENERATOR;

architecture structural of DDS_SINC_GENERATOR is 
  -- Internal Signals
  signal sine_wave, cosine_wave : signed(data_WIDTH - 1 downto 0);
  signal phase : unsigned (cnt_WIDTH - 1 downto 0);
  signal dividend, divisor : integer;
  signal down : std_logic;
begin		 

    -- Intantiate Sin/Cos Generator
    U0: entity DDS_SIN_COS_GENERATOR
    generic map(out_WIDTH => out_WIDTH, pacc_WIDTH => pacc_WIDTH, cnt_WIDTH => cnt_WIDTH, pinc_WIDTH => pinc_WIDTH)
    port map(
        clk => clk,	
        reset => reset,					                
        freq_value => freq_value,
        phase_offset => phase_offset,
        load_freq => load_freq,
        phase_out => phase_out,
        phase_count => phase,	-- Use phase counter as divisor: sinc(x) = sin(x)/x 
        sine_out => sine_wave,
        cosine_out => cosine_wave,
        M_AXIS_tvalid => M_AXIS_tvalid,
        S_AXIS_tvalid => S_AXIS_tvalid
    );
	
    dividend <= to_integer(sine_wave);	-- Treat sine wave value as integer
    divisor <= to_integer(phase_count);	-- Treat sine phase as integer

    process(clk)
      variable max, min, inc : integer;
    begin
      if reset = '1' then
        up_down <= '0';  -- count up
      else 
        inc := to_integer(unsigned(resize(freq_value, 2*data_WIDTH)));
        max := 2**(data_WIDTH - 1) - inc;	-- max phase count before overflow
        min := 0 + inc;				-- min phase

          if divisor >= max then 
            up_down <= not up_down;	-- Reverse counter before overflow
          end if;
		  
    	if up_down = '0' then
	    dividend <= 2**(data_WIDTH - 1) - dividend;	  -- Reverse counter value
	end if;
    end process;
	
    phase_count <= divisor;
		  
    -- sinc(x) = sin(x)/x and then convert back to std_logic_vector 
    sinc_out <= std_logic_vector(unsigned(resize((dividend / divisor), data_WIDTH)));
end DDS_SINC_GENERATOR;
end structural;
