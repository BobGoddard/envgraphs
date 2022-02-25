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

with Ada.Calendar;
with Ada.Strings.Unbounded;
with AdaBase.Statement.Base;
with AdaBase.Driver.Base.MySQL;
with AdaBase.Statement.Base.MySQL;
with Interfaces.C; use Interfaces.C;
with Local_Defs; use Local_Defs;

package DB_Routines is
   subtype  Database_Driver  is AdaBase.Driver.Base.MySQL.MySQL_Driver;
   subtype  Stmt_Type_Local  is AdaBase.Statement.Base.MySQL.MySQL_statement;
   subtype  Stmt_Type_access is AdaBase.Statement.Base.MySQL.MySQL_statement_access;

   function  DB_Connect         return Local_Defs.Trilean;
   procedure DB_Disconnect;
   procedure Execute_Statements;
   function  Get_Earliest_TS    return Interfaces.C.long;
   procedure Load_Data          (DB_Connect_Res : out Local_Defs.Trilean;
                                 Start_Epoch    :     Ada.Calendar.Time;
                                 End_Epoch      :     Ada.Calendar.Time;
                                 DB_Table       :     Ada.Strings.Unbounded.Unbounded_String;
                                 Min_And_Max    : out Local_Defs.MM);
   procedure Load_Locations     (DB_Connect_Res : out Local_Defs.Trilean);
   procedure Set_Account_Details (Host, DB, User, Pass : Ada.Strings.Unbounded.Unbounded_String; Port : Integer := 3306);
private
   function  DB_Connect_Private return Local_Defs.Trilean;
   DB_Host  : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   Database : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   DB_User  : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   DB_Pass  : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   DB_Port  : Integer := 3306;
end DB_Routines;
