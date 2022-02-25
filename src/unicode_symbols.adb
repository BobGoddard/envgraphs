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

with Unicode.CES.Utf8;
with Unicode.Names.Basic_Latin;
with Unicode.Names.Letterlike_Symbols;

package body Unicode_Symbols is
   procedure Setup_Symbols is
      Unicode_Index : Natural := 0;
   begin
      Unicode.CES.Utf8.Encode (Unicode.Names.Letterlike_Symbols.Degree_Celsius, Unicode_Degree, Unicode_Index);

      if Unicode_Index /= 3 then
         raise Program_Error with "Unicode error alpha " & Unicode_Index'Image;
      end if;

      Unicode.CES.Utf8.Encode (Unicode.Names.Basic_Latin.Less_Than_Sign, Unicode_Less_Than, Unicode_Index);

      if Unicode_Index /= 4 then
         raise Program_Error with "Unicode error beta" & Unicode_Index'Image;
      end if;

      Unicode.CES.Utf8.Encode (Unicode.Names.Basic_Latin.Greater_Than_Sign, Unicode_Greater_Than, Unicode_Index);

      if Unicode_Index /= 5 then
         raise Program_Error with "Unicode error gamma" & Unicode_Index'Image;
      end if;
   end Setup_Symbols;
end Unicode_Symbols;
