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

with Local_Defs; use Local_Defs;

package Write_Image_Index is
   procedure Write_HTML_Record (Period           : Local_Defs.Period_Type);
private
   procedure Find_Earliest_File (Start_Directory :        String);
   procedure Find_SVG_File      (Start_Directory :        String;
                                 SVG_Regex       :        String);
   procedure Store_Directory    (Name            :        String;
                                 Store_Index     :        Positive;
                                 Quit            : in out Boolean);
   procedure Update_Start_Time  (Name            :        String;
                                 Store_Index     :        Positive;
                                 Quit            : in out Boolean);
   procedure Write_Header       (Period          :        Local_Defs.Period_Type);
   procedure Write_Data         (Period          :        Local_Defs.Period_Type);
   procedure Write_Tail;
end Write_Image_Index;
