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
with Ada.Calendar.Conversions;
with Ada.Calendar.Time_Zones;
with Ada.Directories;
with Ada.Strings.Maps;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Float_Text_IO;
with Interfaces; use Interfaces;
with Interfaces.C;  use Interfaces.C;
with Interfaces.C.Strings;
with System; use System;
with GNAT.Calendar; use GNAT.Calendar;
with GNAT.Calendar.Time_IO;
with GNAT.Source_Info;
with GNAT.Traceback.Symbolic;
with DOM.Core.Documents;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with Convert;
with List_Handlers;
with Date_Functions;
with Unicode_Symbols;
with Unix_Utils;

package body Construct_SVG is
   procedure Add_Array (GPT : Local_Defs.Graph_Period_Type; ST : Local_Defs.Sensor_Type) is
      Script_Node   :          DOM.Core.Element;
      CData_Node    :          DOM.Core.Cdata_Section;
      UBStr         :          Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("");
      Push_Start_Of : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("Data_Array.push ([");
      Push_End_Of   : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("]);");
      No_Data_Week  : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("'U','No Data'");
      No_Data_Other : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("'U','U','U','No Data'");
   begin
      Script_Node  := DOM.Core.Documents.Create_Element (LDocument, "script");
      DOM.Core.Elements.Set_Attribute (Script_Node, "type", "text/ecmascript");
      Script_Node  := DOM.Core.Nodes.Append_Child (Root_Node, Script_Node);
      CData_Node   := DOM.Core.Documents.Create_Cdata_Section (LDocument, "Data_Array = new Array();" & ASCII.LF);
      CData_Node   := DOM.Core.Nodes.Append_Child (Script_Node, CData_Node);

      for A in 0 .. Local_Defs.Graph_Sizes (GPT).Graph_Width - 1 loop
         if List_Handlers.Tooltip_Map.Contains (A) then
            if GPT in Local_Defs.Graph_Period_Week_Type then
               if ST = Local_Defs.Lux then
                  UBStr := Push_Start_Of & "'" & Convert.Number_To_String (List_Handlers.Tooltip_Map.Element (A).Total_Integer)                                        & "','";
               elsif ST = Local_Defs.Temperature then
                  UBStr := Push_Start_Of & "'" & Convert.Number_To_String (List_Handlers.Tooltip_Map.Element (A).Total_Float)   & " " & Unicode_Symbols.Unicode_Degree & "','";
               else
                  UBStr := Push_Start_Of & "'" & Convert.Number_To_String (List_Handlers.Tooltip_Map.Element (A).Total_Float)                                          & "','";
               end if;
               UBStr := UBStr & List_Handlers.Tooltip_Map.Element (A).Time_Period & "'" & Push_End_Of & ASCII.LF;
            else
               UBStr := Push_Start_Of;
               if ST = Local_Defs.Lux then
                  UBStr := UBStr &
                    "'" & Convert.Number_To_String (List_Handlers.Tooltip_Map.Element (A).Min_Integer)                                                 & "'," &
                    "'" & Convert.Number_To_String (List_Handlers.Tooltip_Map.Element (A).Total_Integer / List_Handlers.Tooltip_Map.Element (A).Count) & "'," &
                    "'" & Convert.Number_To_String (List_Handlers.Tooltip_Map.Element (A).Max_Integer)                                                 & "','";
               elsif ST = Local_Defs.Temperature then
                  UBStr := UBStr &
                    "'" & Convert.Number_To_String (List_Handlers.Tooltip_Map.Element (A).Min_Float)                                                         & " " & Unicode_Symbols.Unicode_Degree & "'," &
                    "'" & Convert.Number_To_String (List_Handlers.Tooltip_Map.Element (A).Total_Float / Float (List_Handlers.Tooltip_Map.Element (A).Count)) & " " & Unicode_Symbols.Unicode_Degree & "'," &
                    "'" & Convert.Number_To_String (List_Handlers.Tooltip_Map.Element (A).Max_Float)                                                         & " " & Unicode_Symbols.Unicode_Degree & "','";
               else
                  UBStr := UBStr &
                    "'" & Convert.Number_To_String (List_Handlers.Tooltip_Map.Element (A).Min_Float)                                                         & "'," &
                    "'" & Convert.Number_To_String (List_Handlers.Tooltip_Map.Element (A).Total_Float / Float (List_Handlers.Tooltip_Map.Element (A).Count)) & "'," &
                    "'" & Convert.Number_To_String (List_Handlers.Tooltip_Map.Element (A).Max_Float)                                                         & "','";
               end if;
               UBStr := UBStr & List_Handlers.Tooltip_Map.Element (A).Time_Period & "'" & Push_End_Of & ASCII.LF;
            end if;
         else
            if GPT in Local_Defs.Graph_Period_Week_Type then
               UBStr := Push_Start_Of & No_Data_Week & Push_End_Of & ASCII.LF;
            else
               UBStr := Push_Start_Of & No_Data_Other & Push_End_Of & ASCII.LF;
            end if;
         end if;

         CData_Node   := DOM.Core.Documents.Create_Cdata_Section (LDocument, Ada.Strings.Unbounded.To_String (UBStr));
         CData_Node   := DOM.Core.Nodes.Append_Child (Script_Node, CData_Node);
      end loop;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Trace_Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet "                      &
                                 GNAT.Source_Info.Source_Location  &
                                 " "                               &
                                 Ada.Exceptions.Exception_Name (E) &
                                 " message: "                      &
                                 Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Trace_Length)));
   end Add_Array;

   procedure Add_CData (GPT : Local_Defs.Graph_Period_Type) is
      Script_Node : DOM.Core.Element;
      CData_Node  : DOM.Core.Cdata_Section;
   begin
      Script_Node  := DOM.Core.Documents.Create_Element (LDocument, "script");
      DOM.Core.Elements.Set_Attribute (Script_Node, "type", "text/ecmascript");
      Script_Node  := DOM.Core.Nodes.Append_Child (Root_Node, Script_Node);

      if GPT = Local_Defs.Week_167
        or else GPT = Local_Defs.Week_168
        or else GPT = Local_Defs.Week_169
      then
         CData_Node   := DOM.Core.Documents.Create_Cdata_Section (LDocument, Local_Defs.CData_Init_Week);
         CData_Node   := DOM.Core.Nodes.Append_Child (Script_Node, CData_Node);
         CData_Node   := DOM.Core.Documents.Create_Cdata_Section (LDocument, Local_Defs.CData_Show_Tooltip_Week);
         CData_Node   := DOM.Core.Nodes.Append_Child (Script_Node, CData_Node);
         CData_Node   := DOM.Core.Documents.Create_Cdata_Section (LDocument, Local_Defs.CData_Hide_Tooltip_Week);
         CData_Node   := DOM.Core.Nodes.Append_Child (Script_Node, CData_Node);
      else
         CData_Node   := DOM.Core.Documents.Create_Cdata_Section (LDocument, Local_Defs.CData_Init_Other);
         CData_Node   := DOM.Core.Nodes.Append_Child (Script_Node, CData_Node);
         CData_Node   := DOM.Core.Documents.Create_Cdata_Section (LDocument, Local_Defs.CData_Show_Tooltip_Other);
         CData_Node   := DOM.Core.Nodes.Append_Child (Script_Node, CData_Node);
         CData_Node   := DOM.Core.Documents.Create_Cdata_Section (LDocument, Local_Defs.CData_Hide_Tooltip_Other);
         CData_Node   := DOM.Core.Nodes.Append_Child (Script_Node, CData_Node);
      end if;
   end Add_CData;

   procedure Add_Data_Other (Meta_Data : Local_Defs.Meta_Data_Type;
                             Graph_Min : Integer;
                             Graph_Max : Integer) is
      Path            :          DOM.Core.Element;
      Circle          :          DOM.Core.Element;
      Path_Str        :          Ada.Strings.Unbounded.Unbounded_String;
      X_Base          : constant Integer := Local_Defs.Graph_Sizes (Meta_Data.GPT).Margin_Left;
      Y_Base          : constant Float   := Float (Local_Defs.Graph_Sizes (Meta_Data.GPT).Margin_Top + Local_Defs.Graph_Sizes (Meta_Data.GPT).Graph_Height - 2);
      Data_Margin     : constant Integer := Graph_Max - Graph_Min;
      XY_Min          :          Local_Defs.XY_Data;
      XY_Max          :          Local_Defs.XY_Data;
   begin
      List_Handlers.XY_Min_Map.Clear;
      List_Handlers.XY_Max_Map.Clear;

      for A in 0 .. Local_Defs.Graph_Sizes (Meta_Data.GPT).Graph_Width loop
         if List_Handlers.Tooltip_Map.Contains (A) then
            XY_Min.X := A;
            XY_Max.X := A;

            if Meta_Data.Sensor_Type = Local_Defs.Lux then
               XY_Min.Y := Y_Base - Float (Local_Defs.Graph_Sizes (Meta_Data.GPT).Graph_Height - 2) * Float (List_Handlers.Tooltip_Map.Element (A).Min_Integer -        Graph_Min)  / Float (Data_Margin);
               XY_Max.Y := Y_Base - Float (Local_Defs.Graph_Sizes (Meta_Data.GPT).Graph_Height - 2) * Float (List_Handlers.Tooltip_Map.Element (A).Max_Integer -        Graph_Min)  / Float (Data_Margin);
            else
               XY_Min.Y := Y_Base - Float (Local_Defs.Graph_Sizes (Meta_Data.GPT).Graph_Height - 2) *       (List_Handlers.Tooltip_Map.Element (A).Min_Float   - Float (Graph_Min)) / Float (Data_Margin);
               XY_Max.Y := Y_Base - Float (Local_Defs.Graph_Sizes (Meta_Data.GPT).Graph_Height - 2) *       (List_Handlers.Tooltip_Map.Element (A).Max_Float   - Float (Graph_Min)) / Float (Data_Margin);
            end if;

            List_Handlers.XY_Min_Map.Insert (XY_Min.X, XY_Min);
            List_Handlers.XY_Max_Map.Insert (XY_Max.X, XY_Max);
         elsif Integer (List_Handlers.XY_Min_Map.Length) /= 0 then
            if Integer (List_Handlers.XY_Min_Map.Length) = 1 and then List_Handlers.XY_Min_Map.First_Element.Y = List_Handlers.XY_Max_Map.First_Element.Y then
               Circle := DOM.Core.Documents.Create_Element (LDocument, "circle");
               DOM.Core.Elements.Set_Attribute (Circle, "cx",           Convert.Number_To_String (List_Handlers.XY_Min_Map.First_Element.X + X_Base));
               DOM.Core.Elements.Set_Attribute (Circle, "cy",           Convert.Number_To_String (List_Handlers.XY_Min_Map.First_Element.Y));
               DOM.Core.Elements.Set_Attribute (Circle, "r",            "0.7");
               DOM.Core.Elements.Set_Attribute (Circle, "stroke-width", "0.7");
               DOM.Core.Elements.Set_Attribute (Circle, "stroke",       "red");
               DOM.Core.Elements.Set_Attribute (Circle, "fill",         "red");
               Circle := DOM.Core.Nodes.Append_Child (Root_Node, Circle);
               List_Handlers.XY_Min_Map.Clear;
               List_Handlers.XY_Max_Map.Clear;
            else
               Path_Str := Ada.Strings.Unbounded.To_Unbounded_String ("M");

               for LMin of         List_Handlers.XY_Min_Map loop
                  Path_Str := Path_Str & " " & Convert.Number_To_String (LMin.X + X_Base) & " " & Convert.Number_To_String (LMin.Y);
               end loop;

               for LMax of reverse List_Handlers.XY_Max_Map loop
                  Path_Str := Path_Str & " " & Convert.Number_To_String (LMax.X + X_Base) & " " & Convert.Number_To_String (LMax.Y);
               end loop;

               Path_Str := Path_Str & " Z";
               Path := DOM.Core.Documents.Create_Element (LDocument,   "path");
               DOM.Core.Elements.Set_Attribute (Path,   "stroke-width",   "1");
               DOM.Core.Elements.Set_Attribute (Path,   "stroke",      "gold");
               DOM.Core.Elements.Set_Attribute (Path,   "fill",        "gold");
               DOM.Core.Elements.Set_Attribute (Path, "d", Ada.Strings.Unbounded.To_String (Path_Str));
               Path     := DOM.Core.Nodes.Append_Child (Root_Node, Path);
               List_Handlers.XY_Min_Map.Clear;
               List_Handlers.XY_Max_Map.Clear;
            end if;
         end if;
      end loop;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Trace_Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location &
                                 " " &
                                 Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Trace_Length)));
   end Add_Data_Other;

   procedure Add_Data_Week (Meta_Data : Local_Defs.Meta_Data_Type;
                            Graph_Min : Integer;
                            Graph_Max : Integer) is
      Path            :          DOM.Core.Element;
      Circle          :          DOM.Core.Element;
      Path_Str        :          Ada.Strings.Unbounded.Unbounded_String;
      X_Base          : constant Integer := Local_Defs.Graph_Sizes (Meta_Data.GPT).Margin_Left;
      Y_Base          : constant Float   := Float (Local_Defs.Graph_Sizes (Meta_Data.GPT).Margin_Top + Local_Defs.Graph_Sizes (Meta_Data.GPT).Graph_Height - 2);
      Data_Margin     : constant Integer := Graph_Max - Graph_Min;
      XY_Min          :          Local_Defs.XY_Data;
   begin
      List_Handlers.XY_Min_Map.Clear;

      for A in 0 .. Local_Defs.Graph_Sizes (Meta_Data.GPT).Graph_Width loop
         if List_Handlers.Tooltip_Map.Contains (A) then
            XY_Min.X := A;

            if Meta_Data.Sensor_Type = Local_Defs.Lux then
               XY_Min.Y := Y_Base - Float (Local_Defs.Graph_Sizes (Meta_Data.GPT).Graph_Height - 2) * Float (List_Handlers.Tooltip_Map.Element (A).Min_Integer -        Graph_Min)  / Float (Data_Margin);
            else
               XY_Min.Y := Y_Base - Float (Local_Defs.Graph_Sizes (Meta_Data.GPT).Graph_Height - 2) *       (List_Handlers.Tooltip_Map.Element (A).Min_Float   - Float (Graph_Min)) / Float (Data_Margin);
            end if;

            List_Handlers.XY_Min_Map.Insert (XY_Min.X, XY_Min);
         elsif Integer (List_Handlers.XY_Min_Map.Length) /= 0 then
            if Integer (List_Handlers.XY_Min_Map.Length) = 1 then
               Circle := DOM.Core.Documents.Create_Element (LDocument, "circle");
               DOM.Core.Elements.Set_Attribute (Circle, "cx",           Convert.Number_To_String (List_Handlers.XY_Min_Map.First_Element.X + X_Base));
               DOM.Core.Elements.Set_Attribute (Circle, "cy",           Convert.Number_To_String (List_Handlers.XY_Min_Map.First_Element.Y));
               DOM.Core.Elements.Set_Attribute (Circle, "r",            "0.7");
               DOM.Core.Elements.Set_Attribute (Circle, "stroke-width", "0.7");
               DOM.Core.Elements.Set_Attribute (Circle, "stroke",       "red");
               DOM.Core.Elements.Set_Attribute (Circle, "fill",         "red");
               Circle := DOM.Core.Nodes.Append_Child (Root_Node, Circle);
               List_Handlers.XY_Min_Map.Clear;
            else
               Path_Str := Ada.Strings.Unbounded.To_Unbounded_String ("M");

               for LMin of         List_Handlers.XY_Min_Map loop
                  Path_Str := Path_Str & " " & Convert.Number_To_String (LMin.X + X_Base) & " " & Convert.Number_To_String (LMin.Y);
               end loop;

               Path := DOM.Core.Documents.Create_Element (LDocument,   "path");
               DOM.Core.Elements.Set_Attribute (Path,   "stroke-width",   "1");
               DOM.Core.Elements.Set_Attribute (Path,   "stroke",      "gold");
               DOM.Core.Elements.Set_Attribute (Path,   "fill",        "none");
               DOM.Core.Elements.Set_Attribute (Path, "d", Ada.Strings.Unbounded.To_String (Path_Str));
               Path     := DOM.Core.Nodes.Append_Child (Root_Node, Path);
               List_Handlers.XY_Min_Map.Clear;
            end if;
         end if;
      end loop;
   end Add_Data_Week;

   procedure Add_Date    (Start_Time : Ada.Calendar.Time;
                          End_Time   : Ada.Calendar.Time;
                          X_Pos      : String;
                          Y_Pos      : String) is
      Text_Node : DOM.Core.Element;
      Text      : DOM.Core.Text;
      LTM_Start : Local_Defs.TM;
      LTM_End   : Local_Defs.TM;
   begin
      GNAT.Calendar.Split_At_Locale (Start_Time, LTM_Start.Year, LTM_Start.Month, LTM_Start.Day, LTM_Start.Hour, LTM_Start.Minute, LTM_Start.Second, LTM_Start.Sub_Second);
      GNAT.Calendar.Split_At_Locale (End_Time,   LTM_End  .Year, LTM_End  .Month, LTM_End  .Day, LTM_End  .Hour, LTM_End  .Minute, LTM_End  .Second, LTM_End  .Sub_Second);
      Text_Node := DOM.Core.Documents.Create_Element   (LDocument, "text");
      DOM.Core.Elements.Set_Attribute                  (Text_Node, "x",            X_Pos);
      DOM.Core.Elements.Set_Attribute                  (Text_Node, "y",            Y_Pos);
      DOM.Core.Elements.Set_Attribute                  (Text_Node, "class", "def-maroon");
      DOM.Core.Elements.Set_Attribute                  (Text_Node, "text-anchor", "left");
      Text_Node := DOM.Core.Nodes.Append_Child         (Root_Node,             Text_Node);
      Text      := DOM.Core.Documents.Create_Text_Node (LDocument,
                                                        Date_Functions.DF_DOW (Start_Time) & " " & Convert.Number_To_String (LTM_Start.Day) & "/" & Convert.Number_To_String (LTM_Start.Month) & "/" & Convert.Number_To_String (LTM_Start.Year mod 100) & " - " &
                                                          Date_Functions.DF_DOW   (End_Time) & " " & Convert.Number_To_String (LTM_End  .Day) & "/" & Convert.Number_To_String (LTM_End  .Month) & "/" & Convert.Number_To_String (LTM_End  .Year mod 100)
                                                       );
      Text      := DOM.Core.Nodes.Append_Child (Text_Node, Text);
   end Add_Date;

   procedure Add_Min_Max (Min_Max_Str : String;
                          X_Pos       : String;
                          Y_Pos       : String) is
      Text_Node : DOM.Core.Element;
      Text      : DOM.Core.Text;
   begin
      Text_Node := DOM.Core.Documents.Create_Element   (LDocument, "text");
      DOM.Core.Elements.Set_Attribute                  (Text_Node, "x",            X_Pos);
      DOM.Core.Elements.Set_Attribute                  (Text_Node, "y",            Y_Pos);
      DOM.Core.Elements.Set_Attribute                  (Text_Node, "class", "def-maroon");
      DOM.Core.Elements.Set_Attribute                  (Text_Node, "text-anchor", "left");
      Text_Node := DOM.Core.Nodes.Append_Child         (Root_Node,             Text_Node);
      Text      := DOM.Core.Documents.Create_Text_Node (LDocument,           Min_Max_Str);
      Text      := DOM.Core.Nodes.Append_Child         (Text_Node,                  Text);
   end Add_Min_Max;

   procedure Add_Rectangle_Inner (Width  : String;
                                  Height : String;
                                  X_Pos  : String;
                                  Y_Pos  : String) is
      Graph_Node         : DOM.Core.Element;
   begin
      Graph_Node  := DOM.Core.Documents.Create_Element (LDocument, "rect");
      DOM.Core.Elements.Set_Attribute                  (Graph_Node, "fill",      "blue");
      DOM.Core.Elements.Set_Attribute                  (Graph_Node, "width",      Width);
      DOM.Core.Elements.Set_Attribute                  (Graph_Node, "height",    Height);
      DOM.Core.Elements.Set_Attribute                  (Graph_Node, "x",          X_Pos);
      DOM.Core.Elements.Set_Attribute                  (Graph_Node, "y",          Y_Pos);
      DOM.Core.Elements.Set_Attribute                  (Graph_Node, "stroke-width", "2");
      DOM.Core.Elements.Set_Attribute                  (Graph_Node, "stroke", "#ff0000");
      Graph_Node  := DOM.Core.Nodes.Append_Child (Root_Node, Graph_Node);
   end Add_Rectangle_Inner;

   procedure Add_Rectangle_Outer (Width, Height : String) is
      Radius_Outer : constant String := "10";
      Graph_Node   :          DOM.Core.Element;
   begin
      Graph_Node := DOM.Core.Documents.Create_Element (LDocument, "rect");
      DOM.Core.Elements.Set_Attribute                 (Graph_Node, "fill",      "#2080dd");
      DOM.Core.Elements.Set_Attribute                 (Graph_Node, "width",         Width);
      DOM.Core.Elements.Set_Attribute                 (Graph_Node, "height",       Height);
      DOM.Core.Elements.Set_Attribute                 (Graph_Node, "rx",     Radius_Outer);
      DOM.Core.Elements.Set_Attribute                 (Graph_Node, "ry",     Radius_Outer);
      DOM.Core.Elements.Set_Attribute                 (Graph_Node, "x",               "0");
      DOM.Core.Elements.Set_Attribute                 (Graph_Node, "y",               "0");
      Graph_Node := DOM.Core.Nodes.Append_Child       (Root_Node,  Graph_Node);
   end Add_Rectangle_Outer;

   procedure Add_Style is
      Style_Node      : DOM.Core.Element;
      Style_Text_Node : DOM.Core.Text;
   begin
      Style_Node      := DOM.Core.Documents.Create_Element   (LDocument,                 "style");
      Style_Node      := DOM.Core.Nodes.Append_Child         (Root_Node,              Style_Node);
      Style_Text_Node := DOM.Core.Documents.Create_Text_Node (LDocument,  Local_Defs.CSS_Classes);
      Style_Text_Node := DOM.Core.Nodes.Append_Child         (Style_Node,        Style_Text_Node);
   end Add_Style;

   procedure Add_Title (Title_Str : Ada.Strings.Unbounded.Unbounded_String;
                        X_Pos     : String;
                        Y_Pos     : String) is
      Text_Node : DOM.Core.Element;
      Text      : DOM.Core.Text;
   begin
      Text_Node := DOM.Core.Documents.Create_Element   (LDocument,                "text");
      DOM.Core.Elements.Set_Attribute                  (Text_Node, "x",            X_Pos);
      DOM.Core.Elements.Set_Attribute                  (Text_Node, "y",            Y_Pos);
      DOM.Core.Elements.Set_Attribute                  (Text_Node, "class", "def-maroon");
      DOM.Core.Elements.Set_Attribute                  (Text_Node, "text-anchor", "left");
      Text_Node := DOM.Core.Nodes.Append_Child         (Root_Node,             Text_Node);
      Text      := DOM.Core.Documents.Create_Text_Node (LDocument, Ada.Strings.Unbounded.To_String (Title_Str));
      Text      := DOM.Core.Nodes.Append_Child         (Text_Node, Text);
   end Add_Title;

   procedure Add_Tooltips (GPT : Local_Defs.Graph_Period_Type) is
      Width          : constant String := Convert.Number_To_String (Local_Defs.Graph_Sizes (GPT).Graph_Width);
      Height         : constant String := Convert.Number_To_String (Local_Defs.Graph_Sizes (GPT).Graph_Height + Local_Defs.Graph_Sizes (GPT).Margin_Top - 1);
      X_Pos          : constant String := Convert.Number_To_String (Local_Defs.Graph_Sizes (GPT).Margin_Left);
      Y_Pos          : constant String := Convert.Number_To_String (Local_Defs.Graph_Sizes (GPT).Margin_Top);
      Element_Parent :          DOM.Core.Element;
      Element_Child  :          DOM.Core.Element;
      TT_Text        :          DOM.Core.Text;
   begin
      Element_Child  := DOM.Core.Documents.Create_Element (LDocument,                     "line");
      DOM.Core.Elements.Set_Attribute                     (Element_Child,          "id", "Oscar");
      DOM.Core.Elements.Set_Attribute                     (Element_Child,     "stroke", "yellow");
      DOM.Core.Elements.Set_Attribute                     (Element_Child,            "x1", "200");
      DOM.Core.Elements.Set_Attribute                     (Element_Child,            "x2", "200");
      DOM.Core.Elements.Set_Attribute                     (Element_Child,            "y1", Y_Pos);
      DOM.Core.Elements.Set_Attribute                     (Element_Child,           "y2", Height);
      DOM.Core.Elements.Set_Attribute                     (Element_Child,    "stroke-width", "1");
      DOM.Core.Elements.Set_Attribute                     (Element_Child, "visibility", "hidden");
      Element_Parent  := DOM.Core.Nodes.Append_Child      (Root_Node,              Element_Child);

      if GPT = Local_Defs.Week_167
        or else GPT = Local_Defs.Week_168
        or else GPT = Local_Defs.Week_169
      then
         Element_Child  := DOM.Core.Documents.Create_Element (LDocument,                     "rect");
         DOM.Core.Elements.Set_Attribute                     (Element_Child,  "class", "Tooltip_BG");
         DOM.Core.Elements.Set_Attribute                     (Element_Child,     "id", "Tooltip_BG");
         DOM.Core.Elements.Set_Attribute                     (Element_Child, "visibility", "hidden");
         DOM.Core.Elements.Set_Attribute                     (Element_Child,             "x", "140");
         DOM.Core.Elements.Set_Attribute                     (Element_Child,              "y", "50");
         DOM.Core.Elements.Set_Attribute                     (Element_Child,         "width", "200");
         DOM.Core.Elements.Set_Attribute                     (Element_Child,         "height", "60");
         DOM.Core.Elements.Set_Attribute                     (Element_Child,              "rx", "5");
         DOM.Core.Elements.Set_Attribute                     (Element_Child,              "ry", "5");
         Element_Parent := DOM.Core.Nodes.Append_Child       (Root_Node,              Element_Child);

         Element_Child := DOM.Core.Documents.Create_Element    (LDocument,                     "text");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,  "class", "def-maroon");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,        "id", "Reading");
         DOM.Core.Elements.Set_Attribute                       (Element_Child, "visibility", "hidden");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,             "x", "150");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,              "y", "75");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,         "style", "red");
         Element_Parent := DOM.Core.Nodes.Append_Child         (Root_Node,              Element_Child);
         TT_Text        := DOM.Core.Documents.Create_Text_Node (LDocument,                         "");
         TT_Text        := DOM.Core.Nodes.Append_Child         (Element_Parent,               TT_Text);

         Element_Child := DOM.Core.Documents.Create_Element    (LDocument,                     "text");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,  "class", "def-maroon");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,         "id", "Timing");
         DOM.Core.Elements.Set_Attribute                       (Element_Child, "visibility", "hidden");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,             "x", "150");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,             "y", "125");
         Element_Parent := DOM.Core.Nodes.Append_Child         (Root_Node,              Element_Child);
         TT_Text        := DOM.Core.Documents.Create_Text_Node (LDocument,                   "Time: ");
         TT_Text        := DOM.Core.Nodes.Append_Child         (Element_Parent,               TT_Text);

      else
         Element_Child  := DOM.Core.Documents.Create_Element (LDocument,                     "rect");
         DOM.Core.Elements.Set_Attribute                     (Element_Child,  "class", "Tooltip_BG");
         DOM.Core.Elements.Set_Attribute                     (Element_Child,     "id", "Tooltip_BG");
         DOM.Core.Elements.Set_Attribute                     (Element_Child, "visibility", "hidden");
         DOM.Core.Elements.Set_Attribute                     (Element_Child,             "x", "140");
         DOM.Core.Elements.Set_Attribute                     (Element_Child,              "y", "50");
         DOM.Core.Elements.Set_Attribute                     (Element_Child,         "width", "200");
         DOM.Core.Elements.Set_Attribute                     (Element_Child,        "height", "110");
         DOM.Core.Elements.Set_Attribute                     (Element_Child,              "rx", "5");
         DOM.Core.Elements.Set_Attribute                     (Element_Child,              "ry", "5");
         Element_Parent := DOM.Core.Nodes.Append_Child       (Root_Node,              Element_Child);
         Element_Child  := DOM.Core.Documents.Create_Element (LDocument,                     "text");

         DOM.Core.Elements.Set_Attribute                       (Element_Child,  "class", "def-maroon");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,     "id", "Sensor_Min");
         DOM.Core.Elements.Set_Attribute                       (Element_Child, "visibility", "hidden");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,             "x", "150");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,              "y", "75");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,         "style", "red");
         Element_Parent := DOM.Core.Nodes.Append_Child         (Root_Node,              Element_Child);
         TT_Text        := DOM.Core.Documents.Create_Text_Node (LDocument,                         "");
         TT_Text        := DOM.Core.Nodes.Append_Child         (Element_Parent,               TT_Text);

         Element_Child := DOM.Core.Documents.Create_Element    (LDocument,                     "text");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,  "class", "def-maroon");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,     "id", "Sensor_Avg");
         DOM.Core.Elements.Set_Attribute                       (Element_Child, "visibility", "hidden");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,             "x", "150");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,             "y", "125");
         Element_Parent := DOM.Core.Nodes.Append_Child         (Root_Node,              Element_Child);
         TT_Text        := DOM.Core.Documents.Create_Text_Node (LDocument,                    "Avg: ");
         TT_Text        := DOM.Core.Nodes.Append_Child         (Element_Parent,               TT_Text);

         Element_Child := DOM.Core.Documents.Create_Element    (LDocument,                     "text");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,  "class", "def-maroon");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,     "id", "Sensor_Max");
         DOM.Core.Elements.Set_Attribute                       (Element_Child, "visibility", "hidden");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,             "x", "150");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,             "y", "100");
         Element_Parent := DOM.Core.Nodes.Append_Child         (Root_Node,              Element_Child);
         TT_Text        := DOM.Core.Documents.Create_Text_Node (LDocument,                    "Max: ");
         TT_Text        := DOM.Core.Nodes.Append_Child         (Element_Parent,               TT_Text);

         Element_Child := DOM.Core.Documents.Create_Element    (LDocument,                     "text");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,  "class", "def-maroon");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,         "id", "Timing");
         DOM.Core.Elements.Set_Attribute                       (Element_Child, "visibility", "hidden");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,             "x", "150");
         DOM.Core.Elements.Set_Attribute                       (Element_Child,             "y", "150");
         Element_Parent := DOM.Core.Nodes.Append_Child         (Root_Node,              Element_Child);
         TT_Text        := DOM.Core.Documents.Create_Text_Node (LDocument,                   "Time: ");
         TT_Text        := DOM.Core.Nodes.Append_Child         (Element_Parent,               TT_Text);
      end if;

      Element_Child  := DOM.Core.Documents.Create_Element (LDocument,                                                                             "rect");
      DOM.Core.Elements.Set_Attribute                     (Element_Child,                                                                "fill", "green");
      DOM.Core.Elements.Set_Attribute                     (Element_Child,                                                          "cursor", "crosshair");
      DOM.Core.Elements.Set_Attribute                     (Element_Child,                                                                "width",  Width);
      DOM.Core.Elements.Set_Attribute                     (Element_Child, "height", Convert.Number_To_String (Local_Defs.Graph_Sizes (GPT).Graph_Height));
      DOM.Core.Elements.Set_Attribute                     (Element_Child,                                                                     "x", X_Pos);
      DOM.Core.Elements.Set_Attribute                     (Element_Child,                                                                     "y", Y_Pos);
      DOM.Core.Elements.Set_Attribute                     (Element_Child,                                                            "stroke-width", "0");
      DOM.Core.Elements.Set_Attribute                     (Element_Child,                                                          "fill-opacity", "0.0");
      DOM.Core.Elements.Set_Attribute                     (Element_Child,                                              "onmousemove", "ShowTooltip(evt)");
      DOM.Core.Elements.Set_Attribute                     (Element_Child,                                               "onmouseout", "HideTooltip(evt)");
      Element_Parent  := DOM.Core.Nodes.Append_Child      (Root_Node,                                                                      Element_Child);
   end Add_Tooltips;

   procedure Add_X_Axis (Meta_Data : Local_Defs.Meta_Data_Type) is
      LTM            :          Local_Defs.TM;
      Start_Of_Day   :          Ada.Calendar.Time := Meta_Data.Date_Start;
      Is_Red         :          Boolean;
      DOW            :          GNAT.Calendar.Day_Name;
      At_Start       :          Interfaces.C.long;
      At_End         :          Interfaces.C.long;
      LDay           :          Ada.Calendar.Day_Number;
      Local_Div      :          Interfaces.C.long;
      Junk           :          Interfaces.C.long;
      Month_End      :          Integer;
      Half_A_Day     : constant Duration := 60.0 * 60.0 * 12.0; -- Adding a day at BST -> GMT only adds 24 hours, rather than the required 25
      Hour_Array     : constant array (1 .. 7) of GNAT.Calendar.Hour_Number := (3, 6, 9, 12, 15, 18, 21);
      subtype Month_31_Array is Month_Name_Long with Static_Predicate => Month_31_Array in January | March | May | July | August | October | December;
   begin
      Local_Div := Local_Defs.Graph_Div (Meta_Data.GPT);

      if Meta_Data.GPT in Local_Defs.Graph_Period_Week_Type or else Meta_Data.GPT in Local_Defs.Graph_Period_Month_Type then
         while Start_Of_Day < Meta_Data.Date_End loop
            GNAT.Calendar.Split_At_Locale (Start_Of_Day, LTM.Year, LTM.Month, LTM.Day, LTM.Hour, LTM.Minute, LTM.Second, LTM.Sub_Second);
            LDay     := LTM.Day;
            DOW      := GNAT.Calendar.Day_Of_Week (Start_Of_Day);
            At_Start := (Ada.Calendar.Conversions.To_Unix_Time (Start_Of_Day) - Ada.Calendar.Conversions.To_Unix_Time (Meta_Data.Date_Start)) / Local_Div;

            if DOW = GNAT.Calendar.Saturday or else DOW = GNAT.Calendar.Sunday then
               Is_Red := True;
            else
               Is_Red := False;
            end if;

            for X of Hour_Array loop
               Add_X_Minor (Meta_Data.GPT, Convert.Number_To_String (Interfaces.C.long (Local_Defs.Graph_Sizes (Meta_Data.GPT).Margin_Left) + (Ada.Calendar.Conversions.To_Unix_Time (
                            GNAT.Calendar.Time_Of_At_Locale (LTM.Year, LTM.Month, LTM.Day,  X, 0, 0, 0.0)) - Ada.Calendar.Conversions.To_Unix_Time (Meta_Data.Date_Start)) / Local_Div));
            end loop;

            Start_Of_Day := GNAT.Calendar.Time_Of_At_Locale (LTM.Year, LTM.Month, LTM.Day, 23, 0, 0, 0.0) + Half_A_Day;  --  Stupid hack...
            GNAT.Calendar.Split_At_Locale (Start_Of_Day, LTM.Year, LTM.Month, LTM.Day, LTM.Hour, LTM.Minute, LTM.Second, LTM.Sub_Second);
            Start_Of_Day := GNAT.Calendar.Time_Of_At_Locale (LTM.Year, LTM.Month, LTM.Day, 0, 0, 0, 0.0);
            At_End    := (Ada.Calendar.Conversions.To_Unix_Time (Start_Of_Day) - Ada.Calendar.Conversions.To_Unix_Time (Meta_Data.Date_Start)) / Local_Div;
            Add_X_Axis_Label (Convert.Number_To_String (LDay),
                              Convert.Number_To_String (Interfaces.C.long (Local_Defs.Graph_Sizes (Meta_Data.GPT).Margin_Left) + At_Start + (At_End - At_Start) / 2),
                              Convert.Number_To_String (Local_Defs.Graph_Sizes (Meta_Data.GPT).Margin_Top + Local_Defs.Graph_Sizes (Meta_Data.GPT).Graph_Height + 20),
                              Is_Red);

            if Start_Of_Day < Meta_Data.Date_End then
               Add_X_Major (Meta_Data.GPT, Convert.Number_To_String (Interfaces.C.long (Local_Defs.Graph_Sizes (Meta_Data.GPT).Margin_Left) + (Ada.Calendar.Conversions.To_Unix_Time (Start_Of_Day) - Ada.Calendar.Conversions.To_Unix_Time (Meta_Data.Date_Start)) / Local_Div));
            end if;
         end loop;
      elsif Meta_Data.GPT in Local_Defs.Graph_Period_Year_Type then  -- Minor per day, major per month
         Is_Red := False;
         GNAT.Calendar.Split_At_Locale (Start_Of_Day, LTM.Year, LTM.Month, LTM.Day, LTM.Hour, LTM.Minute, LTM.Second, LTM.Sub_Second);
         At_Start := 0;

         for Month_Outer in Local_Defs.Month_Name_Long loop
            if Month_Outer = Local_Defs.February then
               if not Date_Functions.Is_Leap (LTM.Year) then -- Non leap year
                  Month_End := 28;
               else
                  Month_End := 29;
               end if;
            elsif Month_Outer  in Month_31_Array then
               Month_End := 31;
            else
               Month_End := 30;
            end if;

            for Month_Date in 2 .. Month_End loop
               Junk := Interfaces.C.long (Local_Defs.Graph_Sizes (Meta_Data.GPT).Margin_Left) +
                 (Ada.Calendar.Conversions.To_Unix_Time (GNAT.Calendar.Time_Of_At_Locale (LTM.Year, Local_Defs.Month_Name_Long'Enum_Rep (Month_Outer), Month_Date,  0, 0, 0, 0.0)) -
                      Ada.Calendar.Conversions.To_Unix_Time (Meta_Data.Date_Start)) / Local_Div;

               if Integer (Ada.Calendar.Time_Zones.Local_Time_Offset (GNAT.Calendar.Time_Of_At_Locale (LTM.Year, Local_Defs.Month_Name_Long'Enum_Rep (Month_Outer), Month_Date,  0, 0, 0, 0.0))) = 60 then
                  Junk := Junk + 1;
               end if;

               Add_X_Minor (Meta_Data.GPT, Convert.Number_To_String (Junk));
            end loop;

            Start_Of_Day := GNAT.Calendar.Time_Of_At_Locale (LTM.Year, Month_Outer'Enum_Rep, 1, 0, 0, 0, 0.0);
            Junk := Interfaces.C.long (Local_Defs.Graph_Sizes (Meta_Data.GPT).Margin_Left) +
              (Ada.Calendar.Conversions.To_Unix_Time (GNAT.Calendar.Time_Of_At_Locale (LTM.Year, Local_Defs.Month_Name_Long'Enum_Rep (Month_Outer), 1,  0, 0, 0, 0.0)) -
                   Ada.Calendar.Conversions.To_Unix_Time (Meta_Data.Date_Start)) / Local_Div;

            if Integer (Ada.Calendar.Time_Zones.Local_Time_Offset (GNAT.Calendar.Time_Of_At_Locale (LTM.Year, Local_Defs.Month_Name_Long'Enum_Rep (Month_Outer), 1,  0, 0, 0, 0.0))) = 60 then
               Junk := Junk + 1;
            end if;

            Add_X_Major (Meta_Data.GPT, Convert.Number_To_String (Junk));

            if Month_Outer = Local_Defs.December then
               Start_Of_Day := GNAT.Calendar.Time_Of_At_Locale (LTM.Year + 1, 1,                           1, 0, 0, 0, 0.0);
            else
               Start_Of_Day := GNAT.Calendar.Time_Of_At_Locale (LTM.Year,     Month_Outer'Enum_Rep + 1,    1, 0, 0, 0, 0.0);
            end if;

            At_End := (Ada.Calendar.Conversions.To_Unix_Time (Start_Of_Day) - Ada.Calendar.Conversions.To_Unix_Time (Meta_Data.Date_Start)) / Local_Div;
            Add_X_Axis_Label (Ada.Strings.Unbounded.To_String (Local_Defs.Long_Month_Names (Month_Outer'Enum_Rep)),
                              Convert.Number_To_String (Interfaces.C.long (Local_Defs.Graph_Sizes (Meta_Data.GPT).Margin_Left) + At_Start + (At_End - At_Start) / 2),
                              Convert.Number_To_String (Local_Defs.Graph_Sizes (Meta_Data.GPT).Margin_Top + Local_Defs.Graph_Sizes (Meta_Data.GPT).Graph_Height + 20),
                              Is_Red);
            At_Start := At_End;
         end loop;

         Start_Of_Day := GNAT.Calendar.Time_Of_At_Locale (LTM.Year + 1, 1, 1, 0, 0, 0, 0.0);
         Junk := Interfaces.C.long (Local_Defs.Graph_Sizes (Meta_Data.GPT).Margin_Left) +
           (Ada.Calendar.Conversions.To_Unix_Time (GNAT.Calendar.Time_Of_At_Locale (LTM.Year + 1, 1, 1,  0, 0, 0, 0.0)) -
              Ada.Calendar.Conversions.To_Unix_Time (Meta_Data.Date_Start)) / Local_Div;

         if Integer (Ada.Calendar.Time_Zones.Local_Time_Offset (GNAT.Calendar.Time_Of_At_Locale (LTM.Year + 1, 1, 1,  0, 0, 0, 0.0))) = 60 then
            Junk := Junk + 1;
         end if;

         Add_X_Major (Meta_Data.GPT, Convert.Number_To_String (Junk));
      end if;
   exception
      when E : others => Ada.Text_IO.Put_Line ("WTF!! rivet "                      &
                                                 GNAT.Source_Info.Source_Location  &
                                                 " "                               &
                                                 Ada.Exceptions.Exception_Name (E) &
                                                 " message: "                      &
                                                 Ada.Exceptions.Exception_Message (E));
   end Add_X_Axis;

   procedure Add_X_Axis_Label (X      : String;
                               X_Pos  : String;
                               Y_Pos  : String;
                               Is_Red : Boolean) is
      Text_Node : DOM.Core.Element;
      Text      : DOM.Core.Text;
   begin
      Text_Node := DOM.Core.Documents.Create_Element   (LDocument,                  "text");
      DOM.Core.Elements.Set_Attribute                  (Text_Node,              "x", X_Pos);
      DOM.Core.Elements.Set_Attribute                  (Text_Node,              "y", Y_Pos);

      if Is_Red then
         DOM.Core.Elements.Set_Attribute               (Text_Node,      "class", "def-red");
      else
         DOM.Core.Elements.Set_Attribute               (Text_Node,   "class", "def-maroon");
      end if;

      DOM.Core.Elements.Set_Attribute                  (Text_Node, "text-anchor", "middle");
      Text_Node := DOM.Core.Nodes.Append_Child         (Root_Node,               Text_Node);
      Text      := DOM.Core.Documents.Create_Text_Node (LDocument,                       X);
      Text      := DOM.Core.Nodes.Append_Child         (Text_Node,                    Text);
   end Add_X_Axis_Label;

   procedure Add_X_Major (Graph_Period : Local_Defs.Graph_Period_Type;
                          X            : String) is
      Bottom_Major : constant Positive := Local_Defs.Graph_Sizes (Graph_Period).Margin_Top + Local_Defs.Graph_Sizes (Graph_Period).Graph_Height + 10;
      Top          : constant Positive := Local_Defs.Graph_Sizes (Graph_Period).Margin_Top;
      Line_Node    :          DOM.Core.Element;
   begin
      Line_Node  := DOM.Core.Documents.Create_Element (LDocument,                                            "line");
      DOM.Core.Elements.Set_Attribute                 (Line_Node,                           "class", "major-X-axis");
      DOM.Core.Elements.Set_Attribute                 (Line_Node,                                           "x1", X);
      DOM.Core.Elements.Set_Attribute                 (Line_Node,                                           "x2", X);
      DOM.Core.Elements.Set_Attribute                 (Line_Node, "y1", Convert.Number_To_String (Bottom_Major - 2));
      DOM.Core.Elements.Set_Attribute                 (Line_Node,              "y2", Convert.Number_To_String (Top));
      Line_Node := DOM.Core.Nodes.Append_Child        (Root_Node,                                         Line_Node);
   end Add_X_Major;

   procedure Add_X_Minor (Graph_Period : Local_Defs.Graph_Period_Type;
                          X            : String) is
      Bottom_Minor : constant Positive := Local_Defs.Graph_Sizes (Graph_Period).Margin_Top + Local_Defs.Graph_Sizes (Graph_Period).Graph_Height;
      Top          : constant Positive := Local_Defs.Graph_Sizes (Graph_Period).Margin_Top;
      Line_Node    :          DOM.Core.Element;
   begin
      Line_Node  := DOM.Core.Documents.Create_Element (LDocument,                                            "line");
      DOM.Core.Elements.Set_Attribute                 (Line_Node, "class",                           "minor-X-axis");
      DOM.Core.Elements.Set_Attribute                 (Line_Node, "x1",                                           X);
      DOM.Core.Elements.Set_Attribute                 (Line_Node, "x2",                                           X);
      DOM.Core.Elements.Set_Attribute                 (Line_Node, "y1", Convert.Number_To_String (Bottom_Minor - 1));
      DOM.Core.Elements.Set_Attribute                 (Line_Node, "y2",              Convert.Number_To_String (Top));
      Line_Node := DOM.Core.Nodes.Append_Child        (Root_Node,                                         Line_Node);
   end Add_X_Minor;

   procedure Add_Y_Axis_Label (Y     : String;
                               X_Pos : String;
                               Y_Pos : String) is
      Text_Node : DOM.Core.Element;
      Text      : DOM.Core.Text;
   begin
      Text_Node := DOM.Core.Documents.Create_Element   (LDocument,                "text");
      DOM.Core.Elements.Set_Attribute                  (Text_Node, "x",            X_Pos);
      DOM.Core.Elements.Set_Attribute                  (Text_Node, "y",            Y_Pos);
      DOM.Core.Elements.Set_Attribute                  (Text_Node, "class", "def-maroon");
      DOM.Core.Elements.Set_Attribute                  (Text_Node, "text-anchor",  "end");
      Text_Node := DOM.Core.Nodes.Append_Child         (Root_Node,             Text_Node);
      Text      := DOM.Core.Documents.Create_Text_Node (LDocument,                     Y);
      Text      := DOM.Core.Nodes.Append_Child         (Text_Node,                  Text);
   end Add_Y_Axis_Label;

   procedure Add_Y_Major (Graph_Period : Local_Defs.Graph_Period_Type;
                          Y            : String) is
      Line_Node   : DOM.Core.Element;
   begin
      Line_Node  := DOM.Core.Documents.Create_Element (LDocument,                  "line");
      DOM.Core.Elements.Set_Attribute                 (Line_Node, "class", "major-Y-axis");
      DOM.Core.Elements.Set_Attribute                 (Line_Node, "x1", Convert.Number_To_String (Local_Defs.Graph_Sizes (Graph_Period).Margin_Left - 10));
      DOM.Core.Elements.Set_Attribute                 (Line_Node, "x2", Convert.Number_To_String (Local_Defs.Graph_Sizes (Graph_Period).Margin_Left + Local_Defs.Graph_Sizes (Graph_Period).Graph_Width - 1));
      DOM.Core.Elements.Set_Attribute                 (Line_Node, "y1",                Y);
      DOM.Core.Elements.Set_Attribute                 (Line_Node, "y2",                Y);
      Line_Node := DOM.Core.Nodes.Append_Child        (Root_Node,              Line_Node);
   end Add_Y_Major;

   procedure Add_Y_Minor (Graph_Period : Local_Defs.Graph_Period_Type;
                          Y            : String) is
      Line_Node   : DOM.Core.Element;
   begin
      Line_Node  := DOM.Core.Documents.Create_Element (LDocument,                  "line");
      DOM.Core.Elements.Set_Attribute                 (Line_Node, "class", "minor-Y-axis");
      DOM.Core.Elements.Set_Attribute                 (Line_Node, "x1", Convert.Number_To_String (Local_Defs.Graph_Sizes (Graph_Period).Margin_Left));
      DOM.Core.Elements.Set_Attribute                 (Line_Node, "x2", Convert.Number_To_String (Local_Defs.Graph_Sizes (Graph_Period).Margin_Left + Local_Defs.Graph_Sizes (Graph_Period).Graph_Width - 1));
      DOM.Core.Elements.Set_Attribute                 (Line_Node,                 "y1", Y);
      DOM.Core.Elements.Set_Attribute                 (Line_Node,                 "y2", Y);
      Line_Node := DOM.Core.Nodes.Append_Child        (Root_Node,               Line_Node);
   end Add_Y_Minor;

   procedure Calculate_Autorange (Min       :     Float;
                                  Max       :     Float;
                                  Graph_Min : out Integer;
                                  Graph_Max : out Integer;
                                  Period    :     Local_Defs.Graph_Period_Type) is
      TMP_Int   : Integer;
      TMP_Float : Float;
      Major     : Float;
      Minor     : Float;
   begin
      if Max - Min < 10.0 then
         Minor := 0.5;
         Major := 2.0;
      elsif Max - Min < 30.0 then
         Minor := 1.0;
         Major := 5.0;
      elsif Max - Min < 50.0 then
         Minor := 1.0;
         Major := 10.0;
      elsif Max - Min < 100.0 then
         Minor := 2.0;
         Major := 10.0;
      elsif Max - Min < 500.0 then
         Minor := 10.0;
         Major := 50.0;
      elsif Max - Min < 1000.0 then
         Minor := 20.0;
         Major := 100.0;
      elsif Max - Min < 2000.0 then
         Minor := 50.0;
         Major := 200.0;
      elsif Max - Min < 8000.0 then
         Minor := 200.0;
         Major := 1_000.0;
      elsif Max - Min < 10_000.0 then
         Minor := 200.0;
         Major := 1_500.0;
      elsif Max - Min < 20_000.0 then
         Minor := 500.0;
         Major := 4_000.0;
      elsif Max - Min < 50_000.0 then
         Minor := 2_000.0;
         Major := 10_000.0;
      else
         Minor := 5_000.0;
         Major := 20_000.0;
      end if;

      if Min < 0.0 then
         Graph_Min := Integer (Min) - Integer (Minor);
      else
         Graph_Min := Integer (Float'Floor (Min)) - Integer (Float'Floor (Min)) mod Integer (Minor);
      end if;

      if Max < 0.0 then
         Graph_Max := Integer (Max) - Integer (Minor) + Integer (Max) mod Integer (Minor);
      else
         Graph_Max := Integer (Float'Floor (Max)) + Integer (Minor) - Integer (Float'Floor (Max)) mod Integer (Minor);
      end if;

      TMP_Int := Graph_Min + Integer (Minor);

      while TMP_Int < Graph_Max loop
         if TMP_Int mod Integer (Major) = 0 then
            TMP_Float     := Float (Local_Defs.Default_Margin_Top - 2 + Local_Defs.Default_Graph_Height) - Float (Local_Defs.Default_Graph_Height - 2) * Float (TMP_Int - Graph_Min) / Float (Graph_Max - Graph_Min);
            Add_Y_Major      (Period, Convert.Number_To_String (TMP_Float));
            Add_Y_Axis_Label (Convert.Number_To_String (TMP_Int),
                              Convert.Number_To_String (Local_Defs.Graph_Sizes (Period).Margin_Left - 13), Convert.Number_To_String (TMP_Float + 7.0));
         else
            Add_Y_Minor (Period,
                         Convert.Number_To_String (Float (Local_Defs.Default_Margin_Top - 2 + Local_Defs.Default_Graph_Height) - Float (Local_Defs.Default_Graph_Height - 2) * Float (TMP_Int - Graph_Min) / Float (Graph_Max - Graph_Min)));
         end if;

         TMP_Int := TMP_Int + Integer (Minor);
      end loop;
   end Calculate_Autorange;

   procedure Populate_Tooltip_Array_Other (Meta_Data : Local_Defs.Meta_Data_Type) is
      X_Div           :          Integer := 0;
      Time_Stamp      :          Interfaces.C.long := Ada.Calendar.Conversions.To_Unix_Time (Meta_Data.Date_Start);
      TZ_Offset_Pre   :          Ada.Calendar.Time_Zones.Time_Offset;
      TZ_Offset_Post  :          Ada.Calendar.Time_Zones.Time_Offset;
      Data_Sensor     :          Local_Defs.Sensor_Data;
      Tooltip_Info    :          Local_Defs.Tooltip_Data;
      TM_Details      :          Unix_Utils.tm;
      TS_Details      :          Unix_Utils.ts;
   begin
      List_Handlers.Tooltip_Map.Clear;
      Reset_Tooltip_Data (Tooltip_Info);
      TZ_Offset_Pre := Ada.Calendar.Time_Zones.Local_Time_Offset (Meta_Data.Date_Start);
      TZ_Offset_Post := TZ_Offset_Pre;

      for Loop_Data of List_Handlers.Sensor_Data_Map loop
         Data_Sensor := Loop_Data;

         if Data_Sensor.TS < Time_Stamp then  -- Still reading data for period
            Update_Fields (Tooltip_Info, Data_Sensor, Meta_Data.Sensor_Type);
         elsif Data_Sensor.TS = Time_Stamp then
            TS_Details.TV_Sec  := Time_Stamp;
            TS_Details.TV_NSec := 0;
            Unix_Utils.localtime_r (TS_Details'Address, TM_Details'Address);
            Update_Fields (Tooltip_Info, Data_Sensor, Meta_Data.Sensor_Type);
            Tooltip_Info.Time_Period := Ada.Strings.Unbounded.To_Unbounded_String (GNAT.Calendar.Time_IO.Image (Ada.Calendar.Conversions.To_Ada_Time (Time_Stamp), "%a %b %d %H:%M ") & Interfaces.C.Strings.Value (TM_Details.tm_zone));
            List_Handlers.Tooltip_Map.Insert (X_Div, Tooltip_Info);
            Reset_Tooltip_Data (Tooltip_Info);
         else
            if Tooltip_Info.Count /= 0 then
               TS_Details.TV_Sec  := Time_Stamp;
               TS_Details.TV_NSec := 0;
               Unix_Utils.localtime_r (TS_Details'Address, TM_Details'Address);
               Tooltip_Info.Time_Period := Ada.Strings.Unbounded.To_Unbounded_String (GNAT.Calendar.Time_IO.Image (Ada.Calendar.Conversions.To_Ada_Time (Time_Stamp), "%a %b %d %H:%M ") & Interfaces.C.Strings.Value (TM_Details.tm_zone));
               List_Handlers.Tooltip_Map.Insert (X_Div, Tooltip_Info);
            end if;

            Reset_Tooltip_Data (Tooltip_Info);
            Update_Fields (Tooltip_Info, Data_Sensor, Meta_Data.Sensor_Type);
         end if;

         while Time_Stamp <= Data_Sensor.TS loop  -- Time_stamp is also < Data_Sensor.TS, so we have to continually increase until at least it is equal
            Time_Stamp  := Time_Stamp + Local_Defs.Graph_Div (Meta_Data.GPT);
            TZ_Offset_Post := Ada.Calendar.Time_Zones.Local_Time_Offset (Ada.Calendar.Conversions.To_Ada_Time (Time_Stamp));

            if Meta_Data.GPT = Local_Defs.Year_365 or else Meta_Data.GPT = Local_Defs.Year_366 then
               if Interfaces.C.long (TZ_Offset_Post) < Interfaces.C.long (TZ_Offset_Pre) then -- BST => GMT
                  Time_Stamp := Time_Stamp + Interfaces.C.long (TZ_Offset_Pre) * 60;
                  TZ_Offset_Pre := TZ_Offset_Post;
               elsif Interfaces.C.long (TZ_Offset_Post) > Interfaces.C.long (TZ_Offset_Pre) then  --  GMT => BST
                  Time_Stamp := Time_Stamp - Interfaces.C.long (TZ_Offset_Post) * 60;
                  TZ_Offset_Pre := TZ_Offset_Post;
               end if;
            end if;
            X_Div := X_Div + 1;
         end loop;
      end loop;

      if Tooltip_Info.Count /= 0 then
         TS_Details.TV_Sec  := Time_Stamp;
         TS_Details.TV_NSec := 0;
         Unix_Utils.localtime_r (TS_Details'Address, TM_Details'Address);
         Tooltip_Info.Time_Period := Ada.Strings.Unbounded.To_Unbounded_String (GNAT.Calendar.Time_IO.Image (Ada.Calendar.Conversions.To_Ada_Time (Time_Stamp), "%a %b %d %H:%M ") & Interfaces.C.Strings.Value (TM_Details.tm_zone));
         List_Handlers.Tooltip_Map.Insert (X_Div, Tooltip_Info);
         Reset_Tooltip_Data (Tooltip_Info);
      end if;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Trace_Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location &
                                 " " &
                                 Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Trace_Length)));
   end Populate_Tooltip_Array_Other;

   procedure Populate_Tooltip_Array_Week (Meta_Data : Local_Defs.Meta_Data_Type) is
      X_Div           :          Integer := 0;
      Time_Stamp      :          Interfaces.C.long := Ada.Calendar.Conversions.To_Unix_Time (Meta_Data.Date_Start);
      Data_Sensor     :          Local_Defs.Sensor_Data;
      Tooltip_Info    :          Local_Defs.Tooltip_Data;
      TM_Details      :          Unix_Utils.tm;
      TS_Details      :          Unix_Utils.ts;
   begin
      List_Handlers.Tooltip_Map.Clear;
      Reset_Tooltip_Data (Tooltip_Info);

      for Loop_Data of List_Handlers.Sensor_Data_Map loop
         Data_Sensor := Loop_Data;

         if Data_Sensor.TS < Time_Stamp then  -- Still reading data for period
            Update_Fields (Tooltip_Info, Data_Sensor, Meta_Data.Sensor_Type);
         elsif Data_Sensor.TS = Time_Stamp then
            Update_Fields (Tooltip_Info, Data_Sensor, Meta_Data.Sensor_Type);
            TS_Details.TV_Sec  := Time_Stamp;
            TS_Details.TV_NSec := 0;
            Unix_Utils.localtime_r (TS_Details'Address, TM_Details'Address);
            Tooltip_Info.Time_Period := Ada.Strings.Unbounded.To_Unbounded_String (GNAT.Calendar.Time_IO.Image (Ada.Calendar.Conversions.To_Ada_Time (Time_Stamp), "%a %b %d %H:%M ") & Interfaces.C.Strings.Value (TM_Details.tm_zone));
            List_Handlers.Tooltip_Map.Insert (X_Div, Tooltip_Info);
            Reset_Tooltip_Data (Tooltip_Info);
         else
            if Tooltip_Info.Count /= 0 then
               TS_Details.TV_Sec  := Time_Stamp;
               TS_Details.TV_NSec := 0;
               Unix_Utils.localtime_r (TS_Details'Address, TM_Details'Address);
               Tooltip_Info.Time_Period := Ada.Strings.Unbounded.To_Unbounded_String (GNAT.Calendar.Time_IO.Image (Ada.Calendar.Conversions.To_Ada_Time (Time_Stamp), "%a %b %d %H:%M ") & Interfaces.C.Strings.Value (TM_Details.tm_zone));
               List_Handlers.Tooltip_Map.Insert (X_Div, Tooltip_Info);
            end if;

            Reset_Tooltip_Data (Tooltip_Info);
            Update_Fields (Tooltip_Info, Data_Sensor, Meta_Data.Sensor_Type);
         end if;

         while Time_Stamp <= Data_Sensor.TS loop  -- Time_stamp is also < Data_Sensor.TS, so we have to continually increase until at least it is equal
            Time_Stamp  := Time_Stamp + Local_Defs.Graph_Div (Meta_Data.GPT);
            X_Div := X_Div + 1;
         end loop;
      end loop;

      if Tooltip_Info.Count /= 0 then
         TS_Details.TV_Sec  := Time_Stamp;
         TS_Details.TV_NSec := 0;
         Unix_Utils.localtime_r (TS_Details'Address, TM_Details'Address);
         Tooltip_Info.Time_Period := Ada.Strings.Unbounded.To_Unbounded_String (GNAT.Calendar.Time_IO.Image (Ada.Calendar.Conversions.To_Ada_Time (Time_Stamp), "%a %b %d %H:%M ") & Interfaces.C.Strings.Value (TM_Details.tm_zone));
         List_Handlers.Tooltip_Map.Insert (X_Div, Tooltip_Info);
         Reset_Tooltip_Data (Tooltip_Info);
      end if;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Trace_Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet " &
                                 GNAT.Source_Info.Source_Location &
                                 " " &
                                 Ada.Exceptions.Exception_Name (E) &
                                 " message: " & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Trace_Length)));
   end Populate_Tooltip_Array_Week;

   procedure Process_All_Data (Meta_Data   : in out Local_Defs.Meta_Data_Type;
                               Min_And_Max :        Local_Defs.MM) is
      SVG_Ext            : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String (".svg");
      Base_Dir           : constant String                                 := "/srv/www/htdocs/env/i";
      Implementation     :          DOM.Core.DOM_Implementation;
      File_Handle        :          Ada.Streams.Stream_IO.File_Type;
      Date_Str           :          String (1 .. 6);
      Dir_Name           :          Ada.Strings.Unbounded.Unbounded_String;
      File_Name          :          Ada.Strings.Unbounded.Unbounded_String;
      Tmp_Min            :          Float;
      Tmp_Max            :          Float;
      Graph_Min          :          Integer;
      Graph_Max          :          Integer;
      CHOwn_Res          :          Interfaces.Integer_32;
      Web_User           :          Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String ("wwwrun");
      Web_PWD            :          Unix_Utils.Password_Entry;
      Buffer_PWD         :          String (1 .. 2 ** 16);
      Buffer_Len         : constant Interfaces.Unsigned_64         := Buffer_PWD'Length;
      Result_PWD         :          System.Address;
      C_Ptr              :          Interfaces.C.Strings.chars_ptr;
   begin
      Ada.Float_Text_IO.Default_Fore    := 0;
      Ada.Float_Text_IO.Default_Aft     := 4;
      Ada.Float_Text_IO.Default_Exp     := 0;

      for Family in Local_Defs.Sensor_Type loop
         if        (Family = Local_Defs.Humidity           and then Min_And_Max.Humidity_Valid)
           or else (Family = Local_Defs.Pressure           and then Min_And_Max.Pressure_Valid)
           or else (Family = Local_Defs.Pressure_Corrected and then Min_And_Max.Pressure_Valid)
           or else (Family = Local_Defs.Temperature        and then Min_And_Max.Temperature_Valid)
           or else (Family = Local_Defs.Lux                and then Min_And_Max.Lux_Valid)
         then

            LDocument := DOM.Core.Create_Document (Implementation);
            Doc_Root_Node := DOM.Core.Documents.Create_Element (LDocument, "svg");
            DOM.Core.Elements.Set_Attribute (Doc_Root_Node, "width",  Convert.Number_To_String (Local_Defs.Graph_Sizes (Meta_Data.GPT).SVG_Width));
            DOM.Core.Elements.Set_Attribute (Doc_Root_Node, "height", Convert.Number_To_String (Local_Defs.Graph_Sizes (Meta_Data.GPT).SVG_Height));
            DOM.Core.Elements.Set_Attribute (Doc_Root_Node, "onload", "init(evt," & Convert.Number_To_String (Local_Defs.Graph_Sizes (Meta_Data.GPT).SVG_Width) & "," & Convert.Number_To_String (Local_Defs.Graph_Sizes (Meta_Data.GPT).SVG_Height) &
                                               "," & Convert.Number_To_String (Local_Defs.Graph_Sizes (Meta_Data.GPT).Margin_Left) & ")");
            DOM.Core.Elements.Set_Attribute (Doc_Root_Node, "xmlns",       "http://www.w3.org/2000/svg");
            DOM.Core.Elements.Set_Attribute (Doc_Root_Node, "xmlns:svg",   "http://www.w3.org/2000/svg");
            DOM.Core.Elements.Set_Attribute (Doc_Root_Node, "xmlns:xlink", "http://www.w3.org/1999/xlink");
            Root_Node             := DOM.Core.Nodes.Append_Child (LDocument, Doc_Root_Node);
            Meta_Data.Sensor_Type := Family;
            Add_Style;
            Add_CData    (Meta_Data.GPT);
            Add_Rectangle_Outer (Convert.Number_To_String (Local_Defs.Graph_Sizes (Meta_Data.GPT).SVG_Width),
                                 Convert.Number_To_String (Local_Defs.Graph_Sizes (Meta_Data.GPT).SVG_Height));
            Add_Rectangle_Inner (Convert.Number_To_String (Local_Defs.Graph_Sizes (Meta_Data.GPT).Graph_Width  + 1),
                                 Convert.Number_To_String (Local_Defs.Graph_Sizes (Meta_Data.GPT).Graph_Height + 1),
                                 Convert.Number_To_String (Local_Defs.Graph_Sizes (Meta_Data.GPT).Margin_Left  - 1),
                                 Convert.Number_To_String (Local_Defs.Graph_Sizes (Meta_Data.GPT).Margin_Top   - 1));
            Add_Date    (Meta_Data.Date_Start, Ada.Calendar.Arithmetic."-" (Meta_Data.Date_End, 1), "10", "380");
            Add_Title   (Meta_Data.Host & " - " & Meta_Data.Location & " " & Meta_Data.Sensor_Name & " - " & (Ada.Strings.Unbounded.Translate (Local_Defs.Sensor_Names (Family), Ada.Strings.Maps.To_Mapping ("_", " "))), "100", "20");
            Add_X_Axis  (Meta_Data);

            case Family is
            when Local_Defs.Humidity           => Tmp_Min :=        Min_And_Max.Min_Humidity;
               Tmp_Max :=        Min_And_Max.Max_Humidity;
            when Local_Defs.Pressure           => Tmp_Min :=        Min_And_Max.Min_Pressure;
               Tmp_Max :=        Min_And_Max.Max_Pressure;
            when Local_Defs.Pressure_Corrected => Tmp_Min :=        Min_And_Max.Min_Pressure_Co;
               Tmp_Max :=        Min_And_Max.Max_Pressure_Co;
            when Local_Defs.Temperature        => Tmp_Min :=        Min_And_Max.Min_Temperature;
               Tmp_Max :=        Min_And_Max.Max_Temperature;
            when Local_Defs.Lux                => Tmp_Min := Float (Min_And_Max.Min_Lux);
               Tmp_Max := Float (Min_And_Max.Max_Lux);
            end case;

            Calculate_Autorange (Tmp_Min, Tmp_Max, Graph_Min, Graph_Max, Meta_Data.GPT);

            if Family = Local_Defs.Temperature then
               Add_Min_Max ("Min: " & Convert.Number_To_String (Tmp_Min) & " " & Unicode_Symbols.Unicode_Degree & ", Max: " & Convert.Number_To_String (Tmp_Max) & " " & Unicode_Symbols.Unicode_Degree, "300", "380");
            else
               Add_Min_Max ("Min: " & Convert.Number_To_String (Tmp_Min)                                        & ", Max: " & Convert.Number_To_String (Tmp_Max),                                        "300", "380");
            end if;

            if Meta_Data.GPT = Local_Defs.Week_167 or else Meta_Data.GPT = Local_Defs.Week_168 or else Meta_Data.GPT = Local_Defs.Week_169 then
               Populate_Tooltip_Array_Week (Meta_Data);
               Add_Data_Week               (Meta_Data, Graph_Min, Graph_Max);
            else
               Populate_Tooltip_Array_Other (Meta_Data);
               Add_Data_Other               (Meta_Data, Graph_Min, Graph_Max);
            end if;

            Add_Array    (Meta_Data.GPT, Meta_Data.Sensor_Type);
            Add_Tooltips (Meta_Data.GPT);
            Date_Str  := Date_Functions.Date_String (Meta_Data.Date_Start);
            Dir_Name  := Base_Dir & "/" & Local_Defs.Period_Names (Meta_Data.Period) & "/" & Local_Defs.Sensor_Names (Family) & "/" & Meta_Data.Sensor_Name;
            File_Name := Date_Str & SVG_Ext;

            if not Ada.Directories.Exists  (Ada.Strings.Unbounded.To_String (Dir_Name)) then
               Ada.Directories.Create_Path (Ada.Strings.Unbounded.To_String (Dir_Name));
            end if;

            if Ada.Directories.Exists      (Ada.Strings.Unbounded.To_String (Dir_Name & "/" & File_Name)) then
               Ada.Directories.Delete_File (Ada.Strings.Unbounded.To_String (Dir_Name & "/" & File_Name));
            end if;

            Ada.Streams.Stream_IO.Create (File_Handle, Name => Ada.Strings.Unbounded.To_String (Dir_Name & "/" & File_Name));
            DOM.Core.Nodes.Write (Ada.Streams.Stream_IO.Stream (File_Handle), LDocument, Pretty_Print => True);
            Ada.Streams.Stream_IO.Close (File_Handle);
            DOM.Core.Nodes.Free (Root_Node);

            if Unix_Utils.getuid = 0 then
               CHOwn_Res := Unix_Utils.Get_PW_Name_R (Web_User,
                                                      Web_PWD'Address,
                                                      Buffer_PWD'Address,
                                                      Buffer_Len,
                                                      Result_PWD'Address);

               if Result_PWD = System.Null_Address or else CHOwn_Res /= 0 then
                  Ada.Text_IO.Put_Line ("Get_PW_Name_R failed, Null returned or CHOwn_Res not zero: " &
                                          Web_PWD.pw_uid'Image                                        &
                                          ", "                                                        &
                                          Web_PWD.pw_gid'Image                                        &
                                          ", "                                                        &
                                          CHOwn_Res'Image);
                  return;
               end if;

               C_Ptr := Interfaces.C.Strings.New_String (Ada.Strings.Unbounded.To_String (Dir_Name & "/" & File_Name));
               CHOwn_Res := Unix_Utils.chown (C_Ptr,
                                              Web_PWD.pw_uid,
                                              Web_PWD.pw_gid);
               Interfaces.C.Strings.Free (C_Ptr);

               if CHOwn_Res /= 0 then
                  Ada.Text_IO.Put_Line ("chown failed: " &
                                          CHOwn_Res'Image);
                  return;
               end if;
            end if;
         end if;
      end loop;

      pragma Warnings (Off, "value might not be referenced");
      Interfaces.C.Strings.Free (Web_User);
      pragma Warnings (On, "value might not be referenced");
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Trace_Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet "                      &
                                 GNAT.Source_Info.Source_Location  &
                                 " "                               &
                                 Ada.Exceptions.Exception_Name (E) &
                                 " message: "                      &
                                 Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Trace_Length)));
   end Process_All_Data;

   procedure Reset_Tooltip_Data (Data : in out Local_Defs.Tooltip_Data) is
   begin
      Data.Min_Float     :=  999_999.0;
      Data.Max_Float     := -999_999.0;
      Data.Total_Float   :=        0.0;
      Data.Min_Integer   :=  999_999;
      Data.Max_Integer   := -999_999;
      Data.Total_Integer :=        0;
      Data.Count         :=        0;
   end Reset_Tooltip_Data;

   procedure Update_Fields (Tooltip_Info : in out Local_Defs.Tooltip_Data;
                            Data_Element :        Local_Defs.Sensor_Data;
                            Sensor       :        Local_Defs.Sensor_Type) is
      TMP_Integer : Integer;
      TMP_Float   : Float;
   begin
      case Sensor is
         when Local_Defs.Lux                => TMP_Integer := Data_Element.Lux;
         when Local_Defs.Humidity           => TMP_Float   := Data_Element.Humidity;
         when Local_Defs.Pressure           => TMP_Float   := Data_Element.Pressure;
         when Local_Defs.Pressure_Corrected => TMP_Float   := Data_Element.Pressure_Co;
         when Local_Defs.Temperature        => TMP_Float   := Data_Element.Temperature;
      end case;

      Tooltip_Info.Count := Tooltip_Info.Count + 1;

      if Sensor = Lux then
         if TMP_Integer < Tooltip_Info.Min_Integer then
            Tooltip_Info.Min_Integer := TMP_Integer;
         end if;

         if TMP_Integer > Tooltip_Info.Max_Integer then
            Tooltip_Info.Max_Integer := TMP_Integer;
         end if;

         Tooltip_Info.Total_Integer := Tooltip_Info.Total_Integer + TMP_Integer;
      else
         if TMP_Float < Tooltip_Info.Min_Float then
            Tooltip_Info.Min_Float := TMP_Float;
         end if;

         if TMP_Float > Tooltip_Info.Max_Float then
            Tooltip_Info.Max_Float := TMP_Float;
         end if;

         Tooltip_Info.Total_Float := Tooltip_Info.Total_Float + TMP_Float;
      end if;
   end Update_Fields;
end Construct_SVG;
