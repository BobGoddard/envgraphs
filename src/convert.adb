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

with Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Strings.Maps;

package body Convert is
   function Float_To_UnBounded (F : Float) return Ada.Strings.Unbounded.Unbounded_String is
      US    :          Ada.Strings.Unbounded.Unbounded_String;
      S     :          String := "                                                ";
      LSet1 : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (' ');
      RSet1 : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ("0");
      RSet2 : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ('.');
   begin
      Ada.Float_Text_IO.Put (S, F);
      US := Ada.Strings.Unbounded.To_Unbounded_String (S);
      Ada.Strings.Unbounded.Trim (US, LSet1, RSet1);
      Ada.Strings.Unbounded.Trim (US, LSet1, RSet2);
      return US;
   end Float_To_UnBounded;

   function Number_To_String (F : Float) return String is
      US    :          Ada.Strings.Unbounded.Unbounded_String;
      S     :          String := "                          ";
      LSet1 : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (' ');
      RSet1 : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ("0");
      RSet2 : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ('.');
   begin
      Ada.Float_Text_IO.Put (S, F);
      US := Ada.Strings.Unbounded.To_Unbounded_String (S);
      Ada.Strings.Unbounded.Trim (US, LSet1, RSet1);
      Ada.Strings.Unbounded.Trim (US, LSet1, RSet2);
      return Ada.Strings.Unbounded.To_String (US);
   end Number_To_String;

   function Number_To_String (I : Integer) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Ada.Strings.Unbounded.Trim (Ada.Strings.Unbounded.To_Unbounded_String (I'Image),
                                              Ada.Strings.Both));
   end Number_To_String;

   function Number_To_String (I : Interfaces.C.long) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Ada.Strings.Unbounded.Trim (Ada.Strings.Unbounded.To_Unbounded_String (I'Image),
                                              Ada.Strings.Both));
   end Number_To_String;

   function  To_Pressure_Corrected (Pressure    : Float;
                                    Temperature : Float) return Float is
      Height                 : constant Float  :=  67.0000000;
      Kelvin                 : constant Float  := 273.1500000;
      Universal_Gas_Constant : constant Float  :=   8.3144598;
      Molar_Mass_Earth_Air   : constant Float  :=   0.0289644;
      Gravity_Acceleration   : constant Float  :=   9.8066500;
      Temperature_Lapse_Rate : constant Float  :=  -0.0065000;
      TKelvin                :          Float;
      Pressure_Corrected     :          Float;
   begin
      TKelvin            := Temperature + Kelvin;
      --    Pressure_Corrected := FPressure * (1.0 - 0.0065 * Height / (FTemperature + 0.0065 * Height + 273.15)) ** (-5.257);
      Pressure_Corrected := Pressure * ((TKelvin + Temperature_Lapse_Rate * (-Height)) / TKelvin) ** ((-Gravity_Acceleration * Molar_Mass_Earth_Air) / (Universal_Gas_Constant * Temperature_Lapse_Rate));
      return Pressure_Corrected;
   end To_Pressure_Corrected;

   function To_Pressure_Corrected (Pressure    : Ada.Strings.Unbounded.Unbounded_String;
                                   Temperature : Ada.Strings.Unbounded.Unbounded_String) return Ada.Strings.Unbounded.Unbounded_String is
      Height                 : constant Float  :=  67.0000000;
      Kelvin                 : constant Float  := 273.1500000;
      Universal_Gas_Constant : constant Float  :=   8.3144598;
      Molar_Mass_Earth_Air   : constant Float  :=   0.0289644;
      Gravity_Acceleration   : constant Float  :=   9.8066500;
      Temperature_Lapse_Rate : constant Float  :=  -0.0065000;
      Tmp_Str                :          String := "          ";
      FPressure              :          Float;
      FTemperature           :          Float;
      TKelvin                :          Float;
      Pressure_Corrected     :          Float;
   begin
      FPressure          := Float'Value (Ada.Strings.Unbounded.To_String (Pressure));
      FTemperature       := Float'Value (Ada.Strings.Unbounded.To_String (Temperature));
      TKelvin            := FTemperature + Kelvin;
      --    Pressure_Corrected := FPressure * (1.0 - 0.0065 * Height / (FTemperature + 0.0065 * Height + 273.15)) ** (-5.257);
      Pressure_Corrected := FPressure * ((TKelvin + Temperature_Lapse_Rate * (-Height)) / TKelvin) ** ((-Gravity_Acceleration * Molar_Mass_Earth_Air) / (Universal_Gas_Constant * Temperature_Lapse_Rate));
      Ada.Float_Text_IO.Put (Tmp_Str,
                             Pressure_Corrected);
      return Ada.Strings.Unbounded.To_Unbounded_String (Tmp_Str);
   end To_Pressure_Corrected;
end Convert;
