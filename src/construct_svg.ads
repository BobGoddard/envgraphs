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

with Ada.Calendar; use Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Traceback;
with DOM.Core;
with Local_Defs;            use Local_Defs;

package Construct_SVG is
   procedure Process_All_Data (Meta_Data                               : in out Local_Defs.Meta_Data_Type;
                               Min_And_Max                             :        Local_Defs.MM);
private
   type CMD is (M, L);
   Current_CMD                                                         :        CMD := M;
   LDocument                                                           :        DOM.Core.Document;
   Root_Node                                                           :        DOM.Core.Element;
   Doc_Root_Node                                                       :        DOM.Core.Element;
   Trace                                                               :        GNAT.Traceback.Tracebacks_Array (1 .. 1_000);
   Trace_Length                                                        :        Natural;
   procedure Add_Array                    (GPT                         :        Local_Defs.Graph_Period_Type;
                                           ST                          :        Local_Defs.Sensor_Type);
   procedure Add_CData                    (GPT                         :        Local_Defs.Graph_Period_Type);
   procedure Add_Data_Other               (Meta_Data                   :        Local_Defs.Meta_Data_Type;
                                           Graph_Min, Graph_Max        :        Integer);
   procedure Add_Data_Week                (Meta_Data                   :        Local_Defs.Meta_Data_Type;
                                           Graph_Min, Graph_Max        :        Integer);
   procedure Add_Date                     (Start_Time                  :        Ada.Calendar.Time;
                                           End_Time                    :        Ada.Calendar.Time;
                                           X_Pos                       :        String;
                                           Y_Pos                       :        String);
   procedure Add_Min_Max                  (Min_Max_Str                 :        String;
                                           X_Pos                       :        String;
                                           Y_Pos                       :        String);
   procedure Add_Rectangle_Inner          (Width                       :        String;
                                           Height                      :        String;
                                           X_Pos                       :        String;
                                           Y_Pos                       :        String);
   procedure Add_Rectangle_Outer          (Width                       :        String;
                                           Height                      :        String);
   procedure Add_Style;
   procedure Add_Title                    (Title_Str                   :        Ada.Strings.Unbounded.Unbounded_String;
                                           X_Pos                       :        String;
                                           Y_Pos                       :        String);
   procedure Add_Tooltips                 (GPT                         :        Local_Defs.Graph_Period_Type);
   procedure Add_X_Axis                   (Meta_Data                   :        Local_Defs.Meta_Data_Type);
   procedure Add_X_Axis_Label             (X                           :        String;
                                           X_Pos                       :        String;
                                           Y_Pos                       :        String;
                                           Is_Red                      :        Boolean);
   procedure Add_X_Major                  (Graph_Period                :        Local_Defs.Graph_Period_Type;
                                           X                           :        String);
   procedure Add_X_Minor                  (Graph_Period                :        Local_Defs.Graph_Period_Type;
                                           X                           :        String);
   procedure Add_Y_Axis_Label             (Y                           :        String;
                                           X_Pos                       :        String;
                                           Y_Pos                       :        String);
   procedure Add_Y_Major                  (Graph_Period                :        Local_Defs.Graph_Period_Type;
                                           Y                           :        String);
   procedure Add_Y_Minor                  (Graph_Period                :        Local_Defs.Graph_Period_Type;
                                           Y                           :        String);
   procedure Calculate_Autorange          (Min                         :        Float;
                                           Max                         :        Float;
                                           Graph_Min                   :    out Integer;
                                           Graph_Max                   :    out Integer;
                                           Period                      :        Local_Defs.Graph_Period_Type);
   procedure Populate_Tooltip_Array_Other (Meta_Data                   :        Local_Defs.Meta_Data_Type);
   procedure Populate_Tooltip_Array_Week  (Meta_Data                   :        Local_Defs.Meta_Data_Type);
   procedure Reset_Tooltip_Data           (Data                        : in out Local_Defs.Tooltip_Data);
   procedure Update_Fields                (Tooltip_Info                : in out Local_Defs.Tooltip_Data;
                                           Data_Element                :        Local_Defs.Sensor_Data;
                                           Sensor                      :        Local_Defs.Sensor_Type);
end Construct_SVG;
