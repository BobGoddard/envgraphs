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

with Ada.Strings.Unbounded;
with GNAT.Traceback;

package Update_Sensors is
   procedure Iterate;
private
   Length                            : Natural;
   Trace                             : GNAT.Traceback.Tracebacks_Array (1 .. 1_000);

   procedure Write_DB_List;
   procedure Write_Details;
   procedure Write_Line (Host        : Ada.Strings.Unbounded.Unbounded_String;
                         Sensor_Name : Ada.Strings.Unbounded.Unbounded_String;
                         Sensor_Type : String;
                         F           : Ada.Strings.Unbounded.Unbounded_String);
   procedure Write_Line (Host        : Ada.Strings.Unbounded.Unbounded_String;
                         Sensor_Name : Ada.Strings.Unbounded.Unbounded_String;
                         Sensor_Type : String;
                         F           : Float);
   procedure Write_Line (Host        : Ada.Strings.Unbounded.Unbounded_String;
                         Sensor_Name : Ada.Strings.Unbounded.Unbounded_String;
                         Sensor_Type : String;
                         I           : Integer);
end Update_Sensors;
