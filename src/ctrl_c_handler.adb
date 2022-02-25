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

package body CTRL_C_Handler is
   CTRL_C          : Boolean := False;

   procedure Monitor_CTRL_C_Called is
   begin
      CTRL_C := True;
   end Monitor_CTRL_C_Called;

   function Monitor_CTRL_C_Is_Called return Boolean is
   begin
      return CTRL_C;
   end Monitor_CTRL_C_Is_Called;
end CTRL_C_Handler;
