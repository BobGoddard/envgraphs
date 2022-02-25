-- Copyright (c) 2021 Bob Goddard <git@1.git.bgcomp.co.uk>
--
-- This file is free software: you may copy, redistribute and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation, either version 2 of the License, or (at your
-- option) any later version.
--
-- This file is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Calendar.Arithmetic;
with Ada.Integer_Text_IO;
with GNAT.Calendar.Time_IO;
with GNAT.Calendar; use GNAT.Calendar;

package body Date_Functions is
   function Date_String (Ada_Time : Ada.Calendar.Time) return String is
      D  : String (1 .. 2) := "00";
      M  : String (1 .. 2) := "00";
      Y  : String (1 .. 2) := "00";
      LTM : Local_Defs.TM;
   begin
      GNAT.Calendar.Split_At_Locale (Ada_Time, LTM.Year, LTM.Month, LTM.Day, LTM.Hour, LTM.Minute, LTM.Second, LTM.Sub_Second);
      Ada.Integer_Text_IO.Put (D, LTM.Day);
      Ada.Integer_Text_IO.Put (M, LTM.Month);
      Ada.Integer_Text_IO.Put (Y, LTM.Year - 2000);

      if D (1) = ' ' then
         D (1) := '0';
      end if;

      if M (1) = ' ' then
         M (1) := '0';
      end if;

      if Y (1) = ' ' then
         Y (1) := '0';
      end if;

      return Y & M & D;
   end Date_String;

   function  DF_DOW (ACT : Ada.Calendar.Time) return String is
      LDOW : GNAT.Calendar.Day_Name;
      DDOW : String (1 .. 3);
   begin
      LDOW := GNAT.Calendar.Day_Of_Week (ACT);

      case LDOW is
         when GNAT.Calendar.Saturday  => DDOW := "Sat";
         when GNAT.Calendar.Sunday    => DDOW := "Sun";
         when GNAT.Calendar.Monday    => DDOW := "Mon";
         when GNAT.Calendar.Tuesday   => DDOW := "Tue";
         when GNAT.Calendar.Wednesday => DDOW := "Wed";
         when GNAT.Calendar.Thursday  => DDOW := "Thu";
         when GNAT.Calendar.Friday    => DDOW := "Fri";
      end case;

      return DDOW;
   end DF_DOW;

   procedure Get_Date_Type (Start_Of_Period : in out Ada.Calendar.Time;
                            Period          :        Local_Defs.Period_Type;
                            End_Of_Period   :    out Ada.Calendar.Time;
                            Graph_Period    :    out Local_Defs.Graph_Period_Type) is
      LDOW : GNAT.Calendar.Day_Name;
      LM   : Local_Defs.TM;
   begin
      GNAT.Calendar.Split_At_Locale (Start_Of_Period,
                                     LM.Year,
                                     LM.Month,
                                     LM.Day,
                                     LM.Hour,
                                     LM.Minute,
                                     LM.Second,
                                     LM.Sub_Second);
      LM.Hour       := 0;
      LM.Minute     := 0;
      LM.Second     := 0;
      LM.Sub_Second := 0.0;

      if Period = Local_Defs.Week then
         Start_Of_Period := GNAT.Calendar.Time_Of_At_Locale (LM.Year,
                                                             LM.Month,
                                                             LM.Day,
                                                             LM.Hour,
                                                             LM.Minute,
                                                             LM.Second,
                                                             LM.Sub_Second);
         LDOW := GNAT.Calendar.Day_Of_Week (Start_Of_Period);

         while LDOW /= GNAT.Calendar.Saturday loop
            Start_Of_Period := Ada.Calendar.Arithmetic."-" (Start_Of_Period, 1);
            LDOW            := GNAT.Calendar.Day_Of_Week   (Start_Of_Period);
         end loop;

         End_Of_Period := Ada.Calendar.Arithmetic."+" (Start_Of_Period, 7);
         GNAT.Calendar.Split_At_Locale (End_Of_Period,
                                        LM.Year,
                                        LM.Month,
                                        LM.Day,
                                        LM.Hour,
                                        LM.Minute,
                                        LM.Second,
                                        LM.Sub_Second);

         if GNAT.Calendar.Day_Of_Week (End_Of_Period) = GNAT.Calendar.Friday then
            End_Of_Period := End_Of_Period + 60.0 * 60.0;
         elsif GNAT.Calendar.Day_Of_Week (End_Of_Period) = GNAT.Calendar.Saturday and then LM.Hour > 0 then
            End_Of_Period := End_Of_Period - 60.0 * 60.0;
         end if;

         if     End_Of_Period - Start_Of_Period = Inc_Week_167 then
            Graph_Period := Local_Defs.Week_167;
         elsif  End_Of_Period - Start_Of_Period = Inc_Week_168 then
            Graph_Period := Local_Defs.Week_168;
         elsif  End_Of_Period - Start_Of_Period = Inc_Week_169 then
            Graph_Period := Local_Defs.Week_169;
         else
            raise Program_Error with "Why can we not find week length, Period: " & Period'Image & ", Start: " & GNAT.Calendar.Time_IO.Image (Start_Of_Period, "%c") & ", End: " & GNAT.Calendar.Time_IO.Image (End_Of_Period, "%c");
         end if;
      elsif Period = Local_Defs.Month then
         LM.Day   := 1;
         Start_Of_Period := GNAT.Calendar.Time_Of_At_Locale (LM.Year,
                                                             LM.Month,
                                                             LM.Day,
                                                             LM.Hour,
                                                             LM.Minute,
                                                             LM.Second,
                                                             LM.Sub_Second);

         if LM.Month = 3 then
            Graph_Period := Local_Defs.Month_31_Spring;
         elsif LM.Month = 10 then
            Graph_Period := Local_Defs.Month_31_Autumn;
         elsif LM.Month = 1 or else LM.Month = 5 or else LM.Month = 7 or else LM.Month = 8 or else LM.Month = 10 or else LM.Month = 12 then
            Graph_Period := Local_Defs.Month_31;
         elsif LM.Month = 4 or else LM.Month = 6 or else LM.Month = 9 or else LM.Month = 11 then
            Graph_Period := Local_Defs.Month_30;
         elsif LM.Month = 2 and then Is_Leap (LM.Year) then
            Graph_Period := Local_Defs.Month_29;
         elsif LM.Month = 2 and then not Is_Leap (LM.Year) then
            Graph_Period := Local_Defs.Month_28;
         else
            raise Program_Error with "Unable to find month length, Period: " & Period'Image & ", Start: " & GNAT.Calendar.Time_IO.Image (Start_Of_Period, "%c") & ", End: " & GNAT.Calendar.Time_IO.Image (End_Of_Period, "%c");
         end if;

         if LM.Month  = Ada.Calendar.Month_Number'Last then
            LM.Month := Ada.Calendar.Month_Number'First;
            LM.Year  := LM.Year + 1;
         else
            LM.Month := LM.Month + 1;
         end if;

         End_Of_Period := GNAT.Calendar.Time_Of_At_Locale (LM.Year,
                                                           LM.Month,
                                                           1,
                                                           0,
                                                           0,
                                                           0,
                                                           0.0);
      elsif Period = Local_Defs.Year then
         LM.Month := 1;
         LM.Day   := 1;
         Start_Of_Period := GNAT.Calendar.Time_Of_At_Locale (LM.Year,
                                                             LM.Month,
                                                             LM.Day,
                                                             LM.Hour,
                                                             LM.Minute,
                                                             LM.Second,
                                                             LM.Sub_Second);
         if Is_Leap (LM.Year) then
            Graph_Period := Local_Defs.Year_366;
         else
            Graph_Period := Local_Defs.Year_365;
         end if;

         End_Of_Period := GNAT.Calendar.Time_Of_At_Locale (LM.Year + 1,
                                                           1,
                                                           1,
                                                           0,
                                                           0,
                                                           0,
                                                           0.0);
      end if;
   end Get_Date_Type;

   function Is_Leap (LYear : Year_Number) return Boolean is
   begin
      if        LYear mod 400 = 0 then
         return True;
      elsif     LYear mod 100 = 0 then
         return False;
      else
         return LYear mod   4 = 0;
      end if;
   end Is_Leap;
end Date_Functions;
