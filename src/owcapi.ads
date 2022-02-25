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

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
with System.CRTL;

package OWCapi is
   function OW_init (arg1 : Interfaces.C.Strings.chars_ptr)       return System.CRTL.ssize_t;  -- owcapi.h:37
   pragma Import (C, OW_init, "OW_init");

   function OW_init_args (arg1 : int; arg2 : System.Address)      return System.CRTL.ssize_t;  -- owcapi.h:38
   pragma Import (C, OW_init_args, "OW_init_args");

   function OW_safe_init (arg1 : Interfaces.C.Strings.chars_ptr)  return System.CRTL.ssize_t;  -- owcapi.h:54
   pragma Import (C, OW_safe_init, "OW_safe_init");

   function OW_safe_init_args (arg1 : int; arg2 : System.Address) return System.CRTL.ssize_t;  -- owcapi.h:55
   pragma Import (C, OW_safe_init_args, "OW_safe_init_args");

   procedure OW_set_error_level (arg1 : Interfaces.C.Strings.chars_ptr);  -- owcapi.h:57
   pragma Import (C, OW_set_error_level, "OW_set_error_level");

   procedure OW_set_error_print (arg1 : Interfaces.C.Strings.chars_ptr);  -- owcapi.h:58
   pragma Import (C, OW_set_error_print, "OW_set_error_print");

   function OW_get
     (arg1 :        Interfaces.C.Strings.chars_ptr;
      arg2 :        System.Address;
      arg3 : access System.CRTL.size_t)                           return System.CRTL.ssize_t;  -- owcapi.h:79
   pragma Import (C, OW_get, "OW_get");

   function OW_present (arg1 : Interfaces.C.Strings.chars_ptr)    return int;  -- owcapi.h:90
   pragma Import (C, OW_present, "OW_present");

   function OW_visible (arg1 : Interfaces.C.Strings.chars_ptr)    return int;  -- owcapi.h:101
   pragma Import (C, OW_visible, "OW_visible");

   function OW_put
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : System.CRTL.size_t)                                  return System.CRTL.ssize_t;  -- owcapi.h:116
   pragma Import (C, OW_put, "OW_put");

   function OW_lread
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : System.CRTL.size_t;
      arg4 : System.CRTL.off_t)                                   return System.CRTL.ssize_t;  -- owcapi.h:128
   pragma Import (C, OW_lread, "OW_lread");

   function OW_lwrite
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : System.CRTL.size_t;
      arg4 : System.CRTL.off_t)                                   return System.CRTL.ssize_t;  -- owcapi.h:140
   pragma Import (C, OW_lwrite, "OW_lwrite");

   procedure OW_finish;  -- owcapi.h:148
   pragma Import (C, OW_finish, "OW_finish");

end OWCapi;
