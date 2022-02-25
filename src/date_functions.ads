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

with Ada.Calendar;  use Ada.Calendar;
with Local_Defs;    use Local_Defs;

package Date_Functions is
   function  Date_String   (Ada_Time        :     Ada.Calendar.Time) return String;
   function  DF_DOW        (ACT             :     Ada.Calendar.Time) return String;
   procedure Get_Date_Type (Start_Of_Period : in out Ada.Calendar.Time;
                            Period          :        Local_Defs.Period_Type;
                            End_Of_Period   :    out Ada.Calendar.Time;
                            Graph_Period    :    out Local_Defs.Graph_Period_Type);
   function  Is_Leap       (LYear           :     Year_Number)       return Boolean;
private
   Inc_Week_167        : constant Duration :=   7.0 * 24.0 * 60.0 * 60.0 - 60.0 * 60.0;
   Inc_Week_168        : constant Duration :=   7.0 * 24.0 * 60.0 * 60.0;
   Inc_Week_169        : constant Duration :=   7.0 * 24.0 * 60.0 * 60.0 + 60.0 * 60.0;
   Inc_Month_28        : constant Duration :=  28.0 * 24.0 * 60.0 * 60.0;
   Inc_Month_29        : constant Duration :=  29.0 * 24.0 * 60.0 * 60.0;
   Inc_Month_30        : constant Duration :=  30.0 * 24.0 * 60.0 * 60.0;
   Inc_Month_31        : constant Duration :=  31.0 * 24.0 * 60.0 * 60.0;
   Inc_Month_31_Spring : constant Duration :=  31.0 * 24.0 * 60.0 * 60.0 - 60.0 * 60.0;
   Inc_Month_31_Autumn : constant Duration :=  31.0 * 24.0 * 60.0 * 60.0 + 60.0 * 60.0;
   Inc_Year_365        : constant Duration := 365.0 * 24.0 * 60.0 * 60.0;
   Inc_Year_366        : constant Duration := 366.0 * 24.0 * 60.0 * 60.0;
end Date_Functions;
