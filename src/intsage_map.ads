--  Copyright (c) 2021 Bob Goddard <git@1.git.bgcomp.co.uk>
--
--  This file is free software: you may copy, redistribute and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 2 of the License, or (at your
--  option) any later version.
--
--  This file is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Containers.Ordered_Maps; use Ada.Containers;
with Interfaces.C; use Interfaces.C;
with Structures; use Structures;

package IntSage_Map is
   package Int_Sage_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Interfaces.C.unsigned_long,
      Element_Type => Structures.Int_Face_Type);
   subtype Int_Cursor is Int_Sage_Map.Cursor;

   package ADSL_Sage_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Interfaces.C.unsigned_long,
      Element_Type => Structures.ADSL_Int_Face_Type);
   subtype ADSL_Cursor is ADSL_Sage_Map.Cursor;
end IntSage_Map;
