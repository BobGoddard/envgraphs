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
with Interfaces.C; use Interfaces.C;
with GNAT.Calendar;

package Local_Defs is
   Do_DB_Update                        : Boolean := False;
   Do_Graphs                           : Boolean := False;
   Do_Indexes                          : Boolean := False;
   Do_Console                          : Boolean := False;

   type Trilean is (TTrue, TFalse, TBroken);
   Do_Syslog                           : Boolean := False;
   Do_Terminal                         : Boolean := True;
   Do_Terminal_Debug                   : Boolean := False;

   type XY_Data is record
      X : Integer;
      Y : Float;
   end record;

   type Tooltip_Data is record
      Min_Float     : Float   := 0.0;
      Max_Float     : Float   := 0.0;
      Total_Float   : Float   := 0.0;
      Min_Integer   : Integer := 0;
      Max_Integer   : Integer := 0;
      Total_Integer : Integer := 0;
      Count         : Integer := 0;
      Time_Period   : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type MM is record
      Min_Humidity,    Max_Humidity    : Float;
      Min_Pressure,    Max_Pressure    : Float;
      Min_Pressure_Co, Max_Pressure_Co : Float;
      Min_Temperature, Max_Temperature : Float;
      Min_Lux,         Max_Lux         : Natural;
      Humidity_Valid                   : Boolean;
      Pressure_Valid                   : Boolean;
      Pressure_Co_Valid                : Boolean;
      Temperature_Valid                : Boolean;
      Lux_Valid                        : Boolean;
   end record;

   type TM is record
      Year                             : Ada. Calendar    .Year_Number;
      Month                            : Ada. Calendar   .Month_Number;
      Day                              : Ada. Calendar     .Day_Number;
      Hour                             : GNAT.Calendar    .Hour_Number;
      Minute                           : GNAT.Calendar  .Minute_Number;
      Second                           : GNAT.Calendar  .Second_Number;
      Sub_Second                       : GNAT.Calendar.Second_Duration;
   end record;

   type Data_Environ is record
      Sensor                           : Ada.Strings.Unbounded.Unbounded_String;
      Location                         : Ada.Strings.Unbounded.Unbounded_String;
      DBTable                          : Ada.Strings.Unbounded.Unbounded_String;
      Host                             : Ada.Strings.Unbounded.Unbounded_String;
      Temperature                      : Ada.Strings.Unbounded.Unbounded_String;
      Pressure                         : Ada.Strings.Unbounded.Unbounded_String;
      Pressure_Corrected               : Ada.Strings.Unbounded.Unbounded_String;
      Humidity                         : Ada.Strings.Unbounded.Unbounded_String;
      Lux                              : Ada.Strings.Unbounded.Unbounded_String;
      Valid                            : Boolean := False;
   end record;

   type Sensor_Data is record
      TS                               : Interfaces.C.long;
      Humidity                         : Float;
      Pressure                         : Float;
      Pressure_Co                      : Float;
      Temperature                      : Float;
      Lux                              : Natural;
   end record;

   type Location_Environ is record
      Sensor                           : Ada.Strings.Unbounded.Unbounded_String;
      Location                         : Ada.Strings.Unbounded.Unbounded_String;
      DBTable                          : Ada.Strings.Unbounded.Unbounded_String;
      Host                             : Ada.Strings.Unbounded.Unbounded_String;
      Temperature                      : Ada.Strings.Unbounded.Unbounded_String;
      Pressure                         : Ada.Strings.Unbounded.Unbounded_String;
      Pressure_Corrected               : Ada.Strings.Unbounded.Unbounded_String;
      Humidity                         : Ada.Strings.Unbounded.Unbounded_String;
      Lux                              : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   subtype Sensor_UString_Type is Ada.Strings.Unbounded.Unbounded_String;
   type    Sensor_Type         is (Humidity, Lux, Pressure, Pressure_Corrected, Temperature);
   subtype Period_UString_Type is Ada.Strings.Unbounded.Unbounded_String;
   type    Period_Type         is (Week, Month, Year);
   subtype Month_Names_Str_Def is String (1 .. 3);

   Sensor_Console_Names : constant array (Sensor_Type) of String (1 .. 13) := ("Humidity:    ",
                                                                               "Lux:         ",
                                                                               "Pressure:    ",
                                                                               "Pressure co: ",
                                                                               "Temperature: ");

   Sensor_Names         : constant array (Sensor_Type) of Sensor_UString_Type := (Ada.Strings.Unbounded.To_Unbounded_String ("Humidity"),
                                                                                  Ada.Strings.Unbounded.To_Unbounded_String ("Lux"),
                                                                                  Ada.Strings.Unbounded.To_Unbounded_String ("Pressure"),
                                                                                  Ada.Strings.Unbounded.To_Unbounded_String ("Pressure_Corrected"),
                                                                                  Ada.Strings.Unbounded.To_Unbounded_String ("Temperature"));

   Period_Names         : constant array (Period_Type) of Period_UString_Type := (Ada.Strings.Unbounded.To_Unbounded_String ("Weekly"),
                                                                                  Ada.Strings.Unbounded.To_Unbounded_String ("Monthly"),
                                                                                  Ada.Strings.Unbounded.To_Unbounded_String ("Yearly"));

   Short_Month_Names    : constant array (Ada.Calendar.Month_Number) of Month_Names_Str_Def :=
                            ("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec");

   Long_Month_Names     : constant array (Ada.Calendar.Month_Number) of Ada.Strings.Unbounded.Unbounded_String :=
                            (Ada.Strings.Unbounded.To_Unbounded_String ("January"),
                             Ada.Strings.Unbounded.To_Unbounded_String ("February"),
                             Ada.Strings.Unbounded.To_Unbounded_String ("March"),
                             Ada.Strings.Unbounded.To_Unbounded_String ("April"),
                             Ada.Strings.Unbounded.To_Unbounded_String ("May"),
                             Ada.Strings.Unbounded.To_Unbounded_String ("June"),
                             Ada.Strings.Unbounded.To_Unbounded_String ("July"),
                             Ada.Strings.Unbounded.To_Unbounded_String ("August"),
                             Ada.Strings.Unbounded.To_Unbounded_String ("September"),
                             Ada.Strings.Unbounded.To_Unbounded_String ("October"),
                             Ada.Strings.Unbounded.To_Unbounded_String ("November"),
                             Ada.Strings.Unbounded.To_Unbounded_String ("December"));

   type Month_Name_Long  is (January, February, March, April, May, June, July, August, September, October, November, December);
   type Month_Name_Short is (Jan,     Feb,      Mar,   Apr,   May, Jun,  Jul,  Aug,    Sep,       Oct,     Nov,      Dec);
   for Month_Name_Long  use (January => 1, February => 2, March => 3, April => 4, May => 5, June => 6, July => 7, August => 8, September => 9, October => 10, November => 11, December => 12);
   for Month_Name_Short use (Jan     => 1, Feb      => 2, Mar   => 3, Apr   => 4, May => 5, Jun  => 6, Jul  => 7, Aug    => 8, Sep       => 9, Oct     => 10, Nov      => 11, Dec      => 12);

   type Graph_Sizes_Type is record
      SVG_Width     : Positive;
      Graph_Width   : Positive;
      Margin_Left   : Positive;
      Margin_Right  : Positive;
      SVG_Height    : Positive;
      Margin_Top    : Positive;
      Graph_Height  : Positive;
      Margin_Bottom : Positive;
      Gap_Minor     : Positive;
   end record;

   -- Week hours, Month days, years days
   type    Graph_Period_Type       is                                      (Week_167,    Week_168,    Week_169,    Month_28,    Month_29,    Month_30,    Month_31,    Month_31_Spring,    Month_31_Autumn,       Year_365,       Year_366);
   Graph_Div   : constant array (Graph_Period_Type) of Interfaces.C.long := (60 * 10,     60 * 10,     60 * 10,     60 * 30,     60 * 30,     60 * 30,     60 * 30,            60 * 30,            60 * 30,    60 * 60 * 6,    60 * 60 * 6);
   subtype Graph_Period_Week_Type  is Graph_Period_Type range Week_167 .. Week_169;
   subtype Graph_Period_Month_Type is Graph_Period_Type range Month_28 .. Month_31_Autumn;
   subtype Graph_Period_Year_Type  is Graph_Period_Type range Year_365 .. Year_366;

   Default_Margin_Left        : constant Positive := 100;
   Default_Margin_Right       : constant Positive :=  30;
   Default_Margin_Top         : constant Positive :=  30;
   Default_Graph_Height       : constant Positive := 300;
   Default_Margin_Bottom      : constant Positive :=  60;
   Default_SVG_Height         : constant Positive := Default_Margin_Top + Default_Graph_Height + Default_Margin_Bottom;
   Points_Week_167            : constant Positive := 7 * 24 * 6 - 6 + 1;
   Points_Week_168            : constant Positive := 7 * 24 * 6 + 1;
   Points_Week_169            : constant Positive := 7 * 24 * 6 + 6 + 1;
   Points_Month_28            : constant Positive := 28 * 24 * 2 + 1;
   Points_Month_29            : constant Positive := 29 * 24 * 2 + 1;
   Points_Month_30            : constant Positive := 30 * 24 * 2 + 1;
   Points_Month_31_Spring     : constant Positive := 31 * 24 * 2 - 2 + 1;
   Points_Month_31            : constant Positive := 31 * 24 * 2 + 1;
   Points_Month_31_Autumn     : constant Positive := 31 * 24 * 2 + 2 + 1;
   Points_Year_365            : constant Positive := 365 * 4 + 1;
   Points_Year_366            : constant Positive := 365 * 4 + 4 + 1;

   --                                                         SVG_Width  Graph_Width             Margin_Left           Margin_Right           SVG_Height           Margin_Top           Graph_Height           Margin_Bottom           Gap_Minor
   Graph_Size_Week_167        : constant Graph_Sizes_Type := (1132,      Points_Week_167,        Default_Margin_Left,  Default_Margin_Right,  Default_SVG_Height,  Default_Margin_Top,  Default_Graph_Height,  Default_Margin_Bottom,  6);  --  Minor every 3 hours
   Graph_Size_Week_168        : constant Graph_Sizes_Type := (1138,      Points_Week_168,        Default_Margin_Left,  Default_Margin_Right,  Default_SVG_Height,  Default_Margin_Top,  Default_Graph_Height,  Default_Margin_Bottom,  6);  --  Minor every 3 hours
   Graph_Size_Week_169        : constant Graph_Sizes_Type := (1144,      Points_Week_169,        Default_Margin_Left,  Default_Margin_Right,  Default_SVG_Height,  Default_Margin_Top,  Default_Graph_Height,  Default_Margin_Bottom,  6);  --  Minor every 3 hours
   Graph_Size_Month_28        : constant Graph_Sizes_Type := (1474,      Points_Month_28,        Default_Margin_Left,  Default_Margin_Right,  Default_SVG_Height,  Default_Margin_Top,  Default_Graph_Height,  Default_Margin_Bottom,  6);  --  Minor every 3 hours
   Graph_Size_Month_29        : constant Graph_Sizes_Type := (1522,      Points_Month_29,        Default_Margin_Left,  Default_Margin_Right,  Default_SVG_Height,  Default_Margin_Top,  Default_Graph_Height,  Default_Margin_Bottom,  6);  --  Minor every 3 hours
   Graph_Size_Month_30        : constant Graph_Sizes_Type := (1570,      Points_Month_30,        Default_Margin_Left,  Default_Margin_Right,  Default_SVG_Height,  Default_Margin_Top,  Default_Graph_Height,  Default_Margin_Bottom,  6);  --  Minor every 3 hours
   Graph_Size_Month_31        : constant Graph_Sizes_Type := (1618,      Points_Month_31,        Default_Margin_Left,  Default_Margin_Right,  Default_SVG_Height,  Default_Margin_Top,  Default_Graph_Height,  Default_Margin_Bottom,  6);  --  Minor every 3 hours
   Graph_Size_Month_31_Spring : constant Graph_Sizes_Type := (1616,      Points_Month_31_Spring, Default_Margin_Left,  Default_Margin_Right,  Default_SVG_Height,  Default_Margin_Top,  Default_Graph_Height,  Default_Margin_Bottom,  6);  --  Minor every 3 hours
   Graph_Size_Month_31_Autumn : constant Graph_Sizes_Type := (1620,      Points_Month_31_Autumn, Default_Margin_Left,  Default_Margin_Right,  Default_SVG_Height,  Default_Margin_Top,  Default_Graph_Height,  Default_Margin_Bottom,  6);  --  Minor every 3 hours
   Graph_Size_Year_365        : constant Graph_Sizes_Type := (1590,      Points_Year_365,        Default_Margin_Left,  Default_Margin_Right,  Default_SVG_Height,  Default_Margin_Top,  Default_Graph_Height,  Default_Margin_Bottom,  8);  --  1 pixel per 6 hours
   Graph_Size_Year_366        : constant Graph_Sizes_Type := (1594,      Points_Year_366,        Default_Margin_Left,  Default_Margin_Right,  Default_SVG_Height,  Default_Margin_Top,  Default_Graph_Height,  Default_Margin_Bottom,  8);  --  1 pixel per 6 hours

   Graph_Sizes                : constant array (Graph_Period_Type) of Graph_Sizes_Type := (Graph_Size_Week_167,
                                                                                           Graph_Size_Week_168,
                                                                                           Graph_Size_Week_169,
                                                                                           Graph_Size_Month_28,
                                                                                           Graph_Size_Month_29,
                                                                                           Graph_Size_Month_30,
                                                                                           Graph_Size_Month_31,
                                                                                           Graph_Size_Month_31_Spring,
                                                                                           Graph_Size_Month_31_Autumn,
                                                                                           Graph_Size_Year_365,
                                                                                           Graph_Size_Year_366);

   subtype Graph_Names_Str_Def is Ada.Strings.Unbounded.Unbounded_String;
   Graph_Names : constant array (Graph_Period_Type) of Graph_Names_Str_Def :=
                   (Ada.Strings.Unbounded.To_Unbounded_String ("Week_167"),
                    Ada.Strings.Unbounded.To_Unbounded_String ("Week_168"),
                    Ada.Strings.Unbounded.To_Unbounded_String ("Week_169"),
                    Ada.Strings.Unbounded.To_Unbounded_String ("Month_28"),
                    Ada.Strings.Unbounded.To_Unbounded_String ("Month_29"),
                    Ada.Strings.Unbounded.To_Unbounded_String ("Month_30"),
                    Ada.Strings.Unbounded.To_Unbounded_String ("Month_31"),
                    Ada.Strings.Unbounded.To_Unbounded_String ("Month_31_Spring"),
                    Ada.Strings.Unbounded.To_Unbounded_String ("Month_31_Autumn"),
                    Ada.Strings.Unbounded.To_Unbounded_String ("Year_365"),
                    Ada.Strings.Unbounded.To_Unbounded_String ("Year_366"));

   type Meta_Data_Type is record
      Date_Start    : Ada.Calendar.Time;
      Date_End      : Ada.Calendar.Time;
      GPT           : Graph_Period_Type;
      Period        : Period_Type;
      Host          : Ada.Strings.Unbounded.Unbounded_String;
      Location      : Ada.Strings.Unbounded.Unbounded_String;
      Sensor_Name   : Ada.Strings.Unbounded.Unbounded_String;
      Sensor_Type   : Local_Defs.Sensor_Type;
   end record;

   Point_Size  : constant String := "14pt;";
   Font        : constant String := "verdana;";
   Radius      : constant String := "5;";
   CSS_Classes : constant String :=                        ASCII.LF &
                   ".tooltip {"                          & ASCII.LF &
                   "   font-size: "         & Point_Size & ASCII.LF &
                   "   font-family: "       & Font       & ASCII.LF &
                   "   }"                                & ASCII.LF &
                   ".Tooltip_BG {"                       & ASCII.LF &
                   "   fill: #D4AF37;"                   & ASCII.LF &
                   "   stroke: black;"                   & ASCII.LF &
                   "   stroke-width: 1;"                 & ASCII.LF &
                   "   opacity: 1;"                      & ASCII.LF &
                   "}"                                   & ASCII.LF &
                   ".minor-X-axis {"                     & ASCII.LF &
                   "   stroke: #6060ff;"                 & ASCII.LF &
                   "   stroke-width: 0.8;"               & ASCII.LF &
                   "}"                                   & ASCII.LF &
                   ".minor-Y-axis {"                     & ASCII.LF &
                   "   stroke: #6060ff;"                 & ASCII.LF &
                   "   stroke-width: 0.8;"               & ASCII.LF &
                   "}"                                   & ASCII.LF &
                   ".major-X-axis {"                     & ASCII.LF &
                   "   stroke: #a0a0ff;"                 & ASCII.LF &
                   "   stroke-width: 1;"                 & ASCII.LF &
                   "}"                                   & ASCII.LF &
                   ".major-Y-axis {"                     & ASCII.LF &
                   "   stroke: #a0a0ff;"                 & ASCII.LF &
                   "   stroke-width: 1;"                 & ASCII.LF &
                   "}"                                   & ASCII.LF &
                   ".def-black {"                        & ASCII.LF &
                   "   fill: black;"                     & ASCII.LF &
                   "   font-family: "       & Font       & ASCII.LF &
                   "   font-size: "         & Point_Size & ASCII.LF &
                   "   stroke: none;"                    & ASCII.LF &
                   "   writing-mode: lr;"                & ASCII.LF &
                   "}"                                   & ASCII.LF &
                   ".def-red {"                          & ASCII.LF &
                   "   fill: red;"                       & ASCII.LF &
                   "   font-family: "       & Font       & ASCII.LF &
                   "   font-size: "         & Point_Size & ASCII.LF &
                   "   stroke: none;"                    & ASCII.LF &
                   "   writing-mode: lr;"                & ASCII.LF &
                   "}"                                   & ASCII.LF &
                   ".def-orange {"                       & ASCII.LF &
                   "   fill: orange;"                    & ASCII.LF &
                   "   font-family: "       & Font       & ASCII.LF &
                   "   font-size: "         & Point_Size & ASCII.LF &
                   "   stroke: none;"                    & ASCII.LF &
                   "   writing-mode: lr;"                & ASCII.LF &
                   "}"                                   & ASCII.LF &
                   ".def-maroon {"                       & ASCII.LF &
                   "   fill: maroon;"                    & ASCII.LF &
                   "   font-family: "       & Font       & ASCII.LF &
                   "   font-size: "         & Point_Size & ASCII.LF &
                   "   stroke: none;"                    & ASCII.LF &
                   "   writing-mode: lr;"                & ASCII.LF &
                   "}"                                   & ASCII.LF &
                   ".data_red {"                         & ASCII.LF &
                   "   fill: #ff0000;"                   & ASCII.LF &
                   "   rx: "                & Radius     & ASCII.LF &
                   "   ry: "                & Radius     & ASCII.LF &
                   "}"                                   & ASCII.LF &
                   ".data_orange {"                      & ASCII.LF &
                   "   fill: #ffa500;"                   & ASCII.LF &
                   "   rx: "                & Radius     & ASCII.LF &
                   "   ry: "                & Radius     & ASCII.LF &
                   "}"                                   & ASCII.LF &
                   ".data_green {"                       & ASCII.LF &
                   "   fill: #00ff00;"                   & ASCII.LF &
                   "   rx: "                & Radius     & ASCII.LF &
                   "   ry: "                & Radius     & ASCII.LF &
                   "}"                                   & ASCII.LF;

   CData_Init_Other         : constant String :=
                                "  function init(evt, sw, sh, offset) {"                                  & ASCII.LF &
                                "    if ( window.svgDocument == null ) {"                                 & ASCII.LF &
                                "      svgDocument = evt.target.ownerDocument;"                           & ASCII.LF &
                                "    }"                                                                   & ASCII.LF &
                                "    SVGWidth           = sw;"                                            & ASCII.LF &
                                "    SVGHeight          = sh;"                                            & ASCII.LF &
                                "    Offset             = offset;"                                        & ASCII.LF &
                                "    Tooltip_BG_ID      = svgDocument.getElementById('Tooltip_BG');"      & ASCII.LF &
                                "    Oscar_ID           = svgDocument.getElementById('Oscar');"           & ASCII.LF &
                                "    Sensor_Min         = svgDocument.getElementById('Sensor_Min');"      & ASCII.LF &
                                "    Sensor_Max         = svgDocument.getElementById('Sensor_Max');"      & ASCII.LF &
                                "    Sensor_Avg         = svgDocument.getElementById('Sensor_Avg');"      & ASCII.LF &
                                "    Timing             = svgDocument.getElementById('Timing');"          & ASCII.LF &
                                "  }";
   CData_Init_Week          : constant String :=
                                "  function init(evt, sw, sh, offset) {"                                  & ASCII.LF &
                                "    if ( window.svgDocument == null ) {"                                 & ASCII.LF &
                                "      svgDocument = evt.target.ownerDocument;"                           & ASCII.LF &
                                "    }"                                                                   & ASCII.LF &
                                "    SVGWidth           = sw;"                                            & ASCII.LF &
                                "    SVGHeight          = sh;"                                            & ASCII.LF &
                                "    Offset             = offset;"                                        & ASCII.LF &
                                "    Tooltip_BG_ID      = svgDocument.getElementById('Tooltip_BG');"      & ASCII.LF &
                                "    Oscar_ID           = svgDocument.getElementById('Oscar');"           & ASCII.LF &
                                "    Reading            = svgDocument.getElementById('Reading');"         & ASCII.LF &
                                "    Timing             = svgDocument.getElementById('Timing');"          & ASCII.LF &
                                "  }";
   CData_Show_Tooltip_Other : constant String :=
                                "  function ShowTooltip(evt) {"                                           & ASCII.LF &
                                "    x = evt.clientX;"                                                    & ASCII.LF &
                                "    y = evt.clientY;"                                                    & ASCII.LF &
                                "    console.log(""X:"" , x, "", Y: "" , y);"                             & ASCII.LF &
                                "    Sensor_Min        .firstChild.data = Data_Array [x - Offset][0];"    & ASCII.LF &
                                "    Sensor_Max        .firstChild.data = Data_Array [x - Offset][1];"    & ASCII.LF &
                                "    Sensor_Avg        .firstChild.data = Data_Array [x - Offset][2];"    & ASCII.LF &
                                "    Timing            .firstChild.data = Data_Array [x - Offset][3];"    & ASCII.LF &
                                "    if (Timing.firstChild.data === 'No Data') {"                         & ASCII.LF &
                                "      HideTooltip(evt)"                                                  & ASCII.LF &
                                "    } else {"                                                            & ASCII.LF &
                                "      length = Math.max( Sensor_Min.getComputedTextLength (), "          &
                                "                         Sensor_Max.getComputedTextLength (), "          &
                                "                         Sensor_Avg.getComputedTextLength (), "          &
                                "                         Timing    .getComputedTextLength ());"          & ASCII.LF &
                                "      NX = x + 20;"                                                      & ASCII.LF &
                                "      if (NX  + length + 20 > SVGWidth) {"                               & ASCII.LF &
                                "        NX = NX - length - 20 - 40;"                                     & ASCII.LF &
                                "      }"                                                                 & ASCII.LF &
                                "      NY = y + 5;"                                                       & ASCII.LF &
                                "      if (NY + 110 > SVGHeight) {"                                       & ASCII.LF &
                                "        NY = NY - 10 - 110;"                                             & ASCII.LF &
                                "      }"                                                                 & ASCII.LF &
                                "      Tooltip_BG_ID     .setAttributeNS(null, 'width', length + 20);"    & ASCII.LF &
                                "      Tooltip_BG_ID     .setAttributeNS(null, 'x',          NX);"        & ASCII.LF &
                                "      Tooltip_BG_ID     .setAttributeNS(null, 'y',          NY);"        & ASCII.LF &
                                "      Tooltip_BG_ID     .setAttributeNS(null, 'visibility', 'visible');" & ASCII.LF &
                                "      Oscar_ID          .setAttributeNS(null, 'visibility', 'visible');" & ASCII.LF &
                                "      Oscar_ID          .setAttributeNS(null,         'x1',         x);" & ASCII.LF &
                                "      Oscar_ID          .setAttributeNS(null,         'x2',         x);" & ASCII.LF &
                                "      Sensor_Min        .setAttributeNS(null, 'visibility', 'visible');" & ASCII.LF &
                                "      Sensor_Min        .setAttributeNS(null, 'x',          NX +  10);"  & ASCII.LF &
                                "      Sensor_Min        .setAttributeNS(null, 'y',          NY +  25);"  & ASCII.LF &
                                "      Sensor_Max        .setAttributeNS(null, 'visibility', 'visible');" & ASCII.LF &
                                "      Sensor_Max        .setAttributeNS(null, 'x',          NX +  10);"  & ASCII.LF &
                                "      Sensor_Max        .setAttributeNS(null, 'y',          NY +  50);"  & ASCII.LF &
                                "      Sensor_Avg        .setAttributeNS(null, 'visibility', 'visible');" & ASCII.LF &
                                "      Sensor_Avg        .setAttributeNS(null, 'x',          NX +  10);"  & ASCII.LF &
                                "      Sensor_Avg        .setAttributeNS(null, 'y',          NY +  75);"  & ASCII.LF &
                                "      Timing            .setAttributeNS(null, 'visibility', 'visible');" & ASCII.LF &
                                "      Timing            .setAttributeNS(null, 'x',          NX +  10);"  & ASCII.LF &
                                "      Timing            .setAttributeNS(null, 'y',          NY + 100);"  & ASCII.LF &
                                "    }"                                                                   & ASCII.LF &
                                "  }";
   CData_Show_Tooltip_Week  : constant String :=
                                "  function ShowTooltip(evt) {"                                           & ASCII.LF &
                                "    x = evt.clientX;"                                                    & ASCII.LF &
                                "    y = evt.clientY;"                                                    & ASCII.LF &
                                "    console.log(""X:"" , x, "", Y: "" , y);"                             & ASCII.LF &
                                "    Reading           .firstChild.data = Data_Array [x - Offset][0];"    & ASCII.LF &
                                "    Timing            .firstChild.data = Data_Array [x - Offset][1];"    & ASCII.LF &
                                "    if (Timing.firstChild.data === 'No Data') {"                         & ASCII.LF &
                                "      HideTooltip(evt)"                                                  & ASCII.LF &
                                "    } else {"                                                            & ASCII.LF &
                                "      length = Math.max( Reading.getComputedTextLength (), "             &
                                "                         Timing .getComputedTextLength ());"             & ASCII.LF &
                                "      NX = x + 20;"                                                      & ASCII.LF &
                                "      if (NX  + length + 20 > SVGWidth) {"                               & ASCII.LF &
                                "        NX = NX - length - 20 - 40;"                                     & ASCII.LF &
                                "      }"                                                                 & ASCII.LF &
                                "      NY = y + 5;"                                                       & ASCII.LF &
                                "      if (NY + 60 > SVGHeight) {"                                        & ASCII.LF &
                                "        NY = NY - 10 - 60;"                                              & ASCII.LF &
                                "      }"                                                                 & ASCII.LF &
                                "      Tooltip_BG_ID     .setAttributeNS(null, 'width', length + 20);"    & ASCII.LF &
                                "      Tooltip_BG_ID     .setAttributeNS(null, 'x',          NX);"        & ASCII.LF &
                                "      Tooltip_BG_ID     .setAttributeNS(null, 'y',          NY);"        & ASCII.LF &
                                "      Tooltip_BG_ID     .setAttributeNS(null, 'visibility', 'visible');" & ASCII.LF &
                                "      Oscar_ID          .setAttributeNS(null, 'visibility', 'visible');" & ASCII.LF &
                                "      Oscar_ID          .setAttributeNS(null,         'x1',         x);" & ASCII.LF &
                                "      Oscar_ID          .setAttributeNS(null,         'x2',         x);" & ASCII.LF &
                                "      Reading           .setAttributeNS(null, 'visibility', 'visible');" & ASCII.LF &
                                "      Reading           .setAttributeNS(null, 'x',          NX +  10);"  & ASCII.LF &
                                "      Reading           .setAttributeNS(null, 'y',          NY +  25);"  & ASCII.LF &
                                "      Timing            .setAttributeNS(null, 'visibility', 'visible');" & ASCII.LF &
                                "      Timing            .setAttributeNS(null, 'x',          NX +  10);"  & ASCII.LF &
                                "      Timing            .setAttributeNS(null, 'y',          NY +  50);"  & ASCII.LF &
                                "    }"                                                                   & ASCII.LF &
                                "  }";
   CData_Hide_Tooltip_Other : constant String :=
                                "  function HideTooltip(evt) {"                                           & ASCII.LF &
                                "    Tooltip_BG_ID     .setAttributeNS(null, 'visibility', 'hidden');"    & ASCII.LF &
                                "    Oscar_ID          .setAttributeNS(null, 'visibility', 'hidden');"    & ASCII.LF &
                                "    Sensor_Min        .setAttributeNS(null, 'visibility', 'hidden');"    & ASCII.LF &
                                "    Sensor_Max        .setAttributeNS(null, 'visibility', 'hidden');"    & ASCII.LF &
                                "    Sensor_Avg        .setAttributeNS(null, 'visibility', 'hidden');"    & ASCII.LF &
                                "    Timing            .setAttributeNS(null, 'visibility', 'hidden');"    & ASCII.LF &
                                "  }";
   CData_Hide_Tooltip_Week  : constant String :=
                                "  function HideTooltip(evt) {"                                           & ASCII.LF &
                                "    Tooltip_BG_ID     .setAttributeNS(null, 'visibility', 'hidden');"    & ASCII.LF &
                                "    Oscar_ID          .setAttributeNS(null, 'visibility', 'hidden');"    & ASCII.LF &
                                "    Reading           .setAttributeNS(null, 'visibility', 'hidden');"    & ASCII.LF &
                                "    Timing            .setAttributeNS(null, 'visibility', 'hidden');"    & ASCII.LF &
                                "  }";

end Local_Defs;
