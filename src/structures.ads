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

with Ada.Strings.Unbounded;
with Interfaces.C;
with Interfaces.C.Strings;

package Structures is
   subtype Int_16_Type  is Interfaces.C.Strings.chars_ptr_array (1 .. 16);

   type ADSL_Int_Face_Type;
   type ADSL_Int_Face_Type_Access is access ADSL_Int_Face_Type;
   type ADSL_Int_Face_Type is record
      ifIndex                                : Interfaces.C.long := 0;  --  INTEGER,                        1
      adslLineCoding                         : Interfaces.C.long := 0;  --  INTEGER                    dmt(2)
      adslLineType                           : Interfaces.C.long := 0;  --  INTEGER               fastOnly(2)
      adslAtucInvVendorID                    : Ada.Strings.Unbounded.Unbounded_String; --  STRING        TSTC
      adslAtucInvVersionNumber               : Ada.Strings.Unbounded.Unbounded_String; --  STRING        1296
      adslAtucCurrSnrMgn                     : Interfaces.C.long := 0;  --  INTEGER                        50
      adslAtucCurrAtn                        : Interfaces.C.long := 0;  --  GUAGE32                       110
      adslAtucCurrOutputPwr                  : Interfaces.C.long := 0;  --  INTEGER                       205
      adslAtucCurrAttainableRate             : Interfaces.C.long := 0;  --  GUAGE32                  16996000
      adslAturInvSerialNumber                : Ada.Strings.Unbounded.Unbounded_String; --  STRING        STMI
      adslAturInvVendorID                    : Ada.Strings.Unbounded.Unbounded_String; --  STRING        STMI
      adslAturInvVersionNumber               : Ada.Strings.Unbounded.Unbounded_String; --  STRING           0
      adslAturCurrSnrMgn                     : Interfaces.C.long := 0;  --  INTEGER                        30
      adslAturCurrAtn                        : Interfaces.C.long := 0;  --  GUAGE32                       230
      adslAturCurrOutputPwr                  : Interfaces.C.long := 0;  --  INTEGER                       125
      adslAturCurrAttainableRate             : Interfaces.C.long := 0;  --  GUAGE32                   1164000
      adslAtucChanCurrTxRate                 : Interfaces.C.long := 0;  --  GUAGE32                  16827000
      adslAturChanCurrTxRate                 : Interfaces.C.long := 0;  --  GUAGE32                    920000
      adslAtucPerfLofs                       : Interfaces.C.long := 0;  --  Counter32                      87
      adslAtucPerfLoss                       : Interfaces.C.long := 0;  --  Counter32                      16
      adslAtucPerfLprs                       : Interfaces.C.long := 0;  --  Counter32                       0
      adslAtucPerfESs                        : Interfaces.C.long := 0;  --  Counter32                      87
      adslAtucPerfInits                      : Interfaces.C.long := 0;  --  Counter32                       8
      adslAtucPerfCurr15MinLofs              : Interfaces.C.long := 0;  --  PerfCurrentCount                0
      adslAtucPerfCurr15MinLoss              : Interfaces.C.long := 0;  --  PerfCurrentCount                0
      adslAtucPerfCurr15MinLprs              : Interfaces.C.long := 0;  --  PerfCurrentCount                0
      adslAtucPerfCurr15MinESs               : Interfaces.C.long := 0;  --  PerfCurrentCount                0
      adslAtucPerfCurr15MinInits             : Interfaces.C.long := 0;  --  PerfCurrentCount                0
      adslAtucPerfCurr1DayLofs               : Interfaces.C.long := 0;  --  adslAtucPerfCurr1DayLofs       87
      adslAtucPerfCurr1DayLoss               : Interfaces.C.long := 0;  --  adslAtucPerfCurr1DayLofs       16
      adslAtucPerfCurr1DayLprs               : Interfaces.C.long := 0;  --  adslAtucPerfCurr1DayLofs        0
      adslAtucPerfCurr1DayESs                : Interfaces.C.long := 0;  --  adslAtucPerfCurr1DayLofs       87
      adslAtucPerfCurr1DayInits              : Interfaces.C.long := 0;  --  adslAtucPerfCurr1DayLofs        5
      adslAturPerfLofs                       : Interfaces.C.long := 0;  --  Counter32                       9
      adslAturPerfLoss                       : Interfaces.C.long := 0;  --  Counter32                       5
      adslAturPerfLprs                       : Interfaces.C.long := 0;  --  Counter32                       0
      adslAturPerfESs                        : Interfaces.C.long := 0;  --  Counter32                  445237
      adslAturPerfCurr15MinLofs              : Interfaces.C.long := 0;  --  PerfCurrentCount                0
      adslAturPerfCurr15MinLoss              : Interfaces.C.long := 0;  --  PerfCurrentCount                0
      adslAturPerfCurr15MinLprs              : Interfaces.C.long := 0;  --  PerfCurrentCount                0
      adslAturPerfCurr15MinESs               : Interfaces.C.long := 0;  --  PerfCurrentCount                6
      adslAturPerfCurr1DayLofs               : Interfaces.C.long := 0;  --  adslAtucPerfCurr1DayLofs   920000
      adslAturPerfCurr1DayLoss               : Interfaces.C.long := 0;  --  adslAtucPerfCurr1DayLofs        5
      adslAturPerfCurr1DayLprs               : Interfaces.C.long := 0;  --  adslAtucPerfCurr1DayLofs        0
      adslAturPerfCurr1DayESs                : Interfaces.C.long := 0;  --  adslAtucPerfCurr1DayLofs     1620
      adslAtucChanCorrectedBlks              : Interfaces.C.long := 0;  --  Counter32                       0
      adslAtucChanUncorrectBlks              : Interfaces.C.long := 0;  --  Counter32                       0
      adslAtucChanPerfCurr15MinCorrectedBlks : Interfaces.C.long := 0;  --  PerfCurrentCount                0
      adslAtucChanPerfCurr15MinUncorrectBlks : Interfaces.C.long := 0;  --  PerfCurrentCount                0
      adslAtucChanPerfCurr1DayCorrectedBlks  : Interfaces.C.long := 0;  --  AdslPerfCurrDayCount            0
      adslAtucChanPerfCurr1DayUncorrectBlks  : Interfaces.C.long := 0;  --  AdslPerfCurrDayCount            0
      adslAturChanCorrectedBlks              : Interfaces.C.long := 0;  --  Counter32                       0
      adslAturChanUncorrectBlks              : Interfaces.C.long := 0;  --  Counter32                 1002529
      adslAturChanPerfCurr15MinCorrectedBlks : Interfaces.C.long := 0;  --  PerfCurrentCount                0
      adslAturChanPerfCurr15MinUncorrectBlks : Interfaces.C.long := 0;  --  PerfCurrentCount                9
      adslAturChanPerfCurr1DayCorrectedBlks  : Interfaces.C.long := 0;  --  AdslPerfCurrDayCount            0
      adslAturChanPerfCurr1DayUncorrectBlks  : Interfaces.C.long := 0;  --  AdslPerfCurrDayCount       487917
   end record;

   type Int_Face_Type;
   type Int_Face_Type_Access is access Int_Face_Type;
   type Int_Face_Type is record
      ifIndex                    : Interfaces.C.long      := 0;            --  INTEGER,    1
      ifDesc                     : Ada.Strings.Unbounded.Unbounded_String; --  String      2
      ifAdminStatus              : Interfaces.C.long      := 0;            --  INTEGER,    7
      ifOperStatus               : Interfaces.C.long      := 0;            --  INTEGER,    8
      ifLastChange               : Interfaces.C.long      := 0;            --  TimeTicks,  9
      ifInOctets                 : Interfaces.C.long      := 0;            --  Counter32, 10
      ifInDiscards               : Interfaces.C.long      := 0;            --  Counter32, 13
      ifInErrors                 : Interfaces.C.long      := 0;            --  Counter32, 14
      ifInUnknownProtos          : Interfaces.C.long      := 0;            --  Counter32, 15
      ifOutOctets                : Interfaces.C.long      := 0;            --  Counter32. 16
      ifOutDiscards              : Interfaces.C.long      := 0;            --  Counter32, 19
      ifOutErrors                : Interfaces.C.long      := 0;            --  Counter32, 20

      ifName                     : Ada.Strings.Unbounded.Unbounded_String; --  String      1
      ifInMulticastPkts          : Interfaces.C.long      := 0;            --  Counter32,  2
      ifInBroadcastPkts          : Interfaces.C.long      := 0;            --  Counter32,  3
      ifOutMulticastPkts         : Interfaces.C.long      := 0;            --  Counter32,  4
      ifOutBroadcastPkts         : Interfaces.C.long      := 0;            --  Counter32,  5
      ifHCInOctets               : Interfaces.Unsigned_64 := 0;            --  Counter64,  6
      ifHCInUcastPkts            : Interfaces.Unsigned_64 := 0;            --  Counter64,  7
      ifHCInMulticastPkts        : Interfaces.Unsigned_64 := 0;            --  Counter64,  8
      ifHCInBroadcastPkts        : Interfaces.Unsigned_64 := 0;            --  Counter64,  9
      ifHCOutOctets              : Interfaces.Unsigned_64 := 0;            --  Counter64, 10
      ifHCOutUcastPkts           : Interfaces.Unsigned_64 := 0;            --  Counter64, 11
      ifHCOutMulticastPkts       : Interfaces.Unsigned_64 := 0;            --  Counter64, 12
      ifHCOutBroadcastPkts       : Interfaces.Unsigned_64 := 0;            --  Counter64, 13
      ifCounterDiscontinuityTime : Interfaces.C.long      := 0;            --  TimeStamp  19
   end record;
end Structures;
