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

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Ordered_Sets; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with Local_Defs; use Local_Defs;

package List_Handlers is
   package Sensor_Data_Map_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     =>   Interfaces.C.long,
      Element_Type => Local_Defs.Sensor_Data);

   package Tooltip_Map_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     =>   Integer,
      Element_Type => Local_Defs.Tooltip_Data);

   package Data_Environ_Map_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type => Local_Defs.Data_Environ);

   package Location_Environ_Map_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type => Local_Defs.Location_Environ);

   package XY_Map_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     =>   Integer,
      Element_Type => Local_Defs.XY_Data);

   package Dir_Ordered_Set         is new Ada.Containers.Ordered_Sets                  (Element_Type     => Ada.Strings.Unbounded.Unbounded_String);
   package File_Ordered_Set        is new Ada.Containers.Ordered_Sets                  (Element_Type     => Ada.Strings.Unbounded.Unbounded_String);
   package SQL_Map_Queue_Interface is new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type     => Ada.Strings.Unbounded.Unbounded_String);
   package SQL_Map_Queue           is new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => SQL_Map_Queue_Interface);

   Data_Environ_Map     :     Data_Environ_Map_Package.Map;
   Location_Environ_Map : Location_Environ_Map_Package.Map;
   Sensor_Data_Map      :      Sensor_Data_Map_Package.Map;
   XY_Min_Map           :               XY_Map_Package.Map;
   XY_Max_Map           :               XY_Map_Package.Map;
   Tooltip_Map          :          Tooltip_Map_Package.Map;
   Directory_Set        :              Dir_Ordered_Set.Set;
   File_Set             :             File_Ordered_Set.Set;
   SQL_Queue            :              SQL_Map_Queue.Queue;
end List_Handlers;
