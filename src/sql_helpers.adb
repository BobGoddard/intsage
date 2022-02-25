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

with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Traceback.Symbolic;
with GNAT.Source_Info;
with GNATCOLL.Traces.Syslog;

package body SQL_Helpers is
   SQL    : Ada.Strings.Unbounded.Unbounded_String;
   Comma  : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String (",");
   Quotes       : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("""");
   Trace        : GNAT.Traceback.Tracebacks_Array (1 .. 1_000);
   Length       : Natural;

   function ADSL_SQL (TS : Interfaces.C.long; ADSL : Structures.ADSL_Int_Face_Type) return Ada.Strings.Unbounded.Unbounded_String is
   begin
      SQL := Ada.Strings.Unbounded.To_Unbounded_String ("INSERT INTO counters_adsl ("                                                                                              &
                                                          "ts, ifIndex, adslLineCoding, adslLineType, adslAtucInvVendorID, adslAtucInvVersionNumber, adslAturInvVendorID,"         &
                                                          "adslAturInvVersionNumber, adslAturInvSerialNumber, adslAtucCurrSnrMgn, adslAtucCurrAtn, adslAtucCurrOutputPwr,"         &
                                                          "adslAtucCurrAttainableRate, adslAturCurrSnrMgn, adslAturCurrAtn, adslAturCurrOutputPwr, adslAturCurrAttainableRate,"    &
                                                          "adslAtucChanCurrTxRate, adslAturChanCurrTxRate, adslAtucPerfLofs, adslAtucPerfLoss, adslAtucPerfLprs,"                  &
                                                          "adslAtucPerfESs, adslAtucPerfInits, adslAtucPerfCurr15MinLofs, adslAtucPerfCurr15MinLoss, adslAtucPerfCurr15MinLprs,"   &
                                                          "adslAtucPerfCurr15MinESs, adslAtucPerfCurr15MinInits, adslAtucPerfCurr1DayLofs, adslAtucPerfCurr1DayLoss,"              &
                                                          "adslAtucPerfCurr1DayLprs, adslAtucPerfCurr1DayESs, adslAtucPerfCurr1DayInits, adslAturPerfLofs, adslAturPerfLoss,"      &
                                                          "adslAturPerfLprs, adslAturPerfESs, adslAturPerfCurr15MinLofs, adslAturPerfCurr15MinLoss, adslAturPerfCurr15MinLprs,"    &
                                                          "adslAturPerfCurr15MinESs, adslAturPerfCurr1DayLofs, adslAturPerfCurr1DayLoss, adslAturPerfCurr1DayLprs,"                &
                                                          "adslAturPerfCurr1DayESs, adslAtucChanCorrectedBlks, adslAtucChanUncorrectBlks, adslAtucChanPerfCurr15MinCorrectedBlks," &
                                                          "adslAtucChanPerfCurr15MinUncorrectBlks, adslAtucChanPerfCurr1DayCorrectedBlks, adslAtucChanPerfCurr1DayUncorrectBlks,"  &
                                                          "adslAturChanCorrectedBlks, adslAturChanUncorrectBlks, adslAturChanPerfCurr15MinCorrectedBlks,"                          &
                                                          "adslAturChanPerfCurr15MinUncorrectBlks, adslAturChanPerfCurr1DayCorrectedBlks, adslAturChanPerfCurr1DayUncorrectBlks"   &
                                                          ") values (");
      SQL := SQL & TS'Image                               & Comma &
        ADSL.ifIndex'Image                                & Comma &
        ADSL.adslLineCoding'Image                         & Comma &
        ADSL.adslLineType'Image                           & Comma &
        Quotes & ADSL.adslAtucInvVendorID        & Quotes & Comma &
        Quotes & ADSL.adslAtucInvVersionNumber   & Quotes & Comma &
        Quotes & ADSL.adslAturInvVendorID        & Quotes & Comma &
        Quotes & ADSL.adslAturInvVersionNumber   & Quotes & Comma &
        Quotes & ADSL.adslAturInvSerialNumber    & Quotes & Comma &
        ADSL.adslAtucCurrSnrMgn'Image                     & Comma &
        ADSL.adslAtucCurrAtn'Image                        & Comma &
        ADSL.adslAtucCurrOutputPwr'Image                  & Comma &
        ADSL.adslAtucCurrAttainableRate'Image             & Comma &
        ADSL.adslAturCurrSnrMgn'Image                     & Comma &
        ADSL.adslAturCurrAtn'Image                        & Comma &
        ADSL.adslAturCurrOutputPwr'Image                  & Comma &
        ADSL.adslAturCurrAttainableRate'Image             & Comma &
        ADSL.adslAtucChanCurrTxRate'Image                 & Comma &
        ADSL.adslAturChanCurrTxRate'Image                 & Comma &
        ADSL.adslAtucPerfLofs'Image                       & Comma &
        ADSL.adslAtucPerfLoss'Image                       & Comma &
        ADSL.adslAtucPerfLprs'Image                       & Comma &
        ADSL.adslAtucPerfESs'Image                        & Comma &
        ADSL.adslAtucPerfInits'Image                      & Comma &
        ADSL.adslAtucPerfCurr15MinLofs'Image              & Comma &
        ADSL.adslAtucPerfCurr15MinLoss'Image              & Comma &
        ADSL.adslAtucPerfCurr15MinLprs'Image              & Comma &
        ADSL.adslAtucPerfCurr15MinESs'Image               & Comma &
        ADSL.adslAtucPerfCurr15MinInits'Image             & Comma &
        ADSL.adslAtucPerfCurr1DayLofs'Image               & Comma &
        ADSL.adslAtucPerfCurr1DayLoss'Image               & Comma &
        ADSL.adslAtucPerfCurr1DayLprs'Image               & Comma &
        ADSL.adslAtucPerfCurr1DayESs'Image                & Comma &
        ADSL.adslAtucPerfCurr1DayInits'Image              & Comma &
        ADSL.adslAturPerfLofs'Image                       & Comma &
        ADSL.adslAturPerfLoss'Image                       & Comma &
        ADSL.adslAturPerfLprs'Image                       & Comma &
        ADSL.adslAturPerfESs'Image                        & Comma &
        ADSL.adslAturPerfCurr15MinLofs'Image              & Comma &
        ADSL.adslAturPerfCurr15MinLoss'Image              & Comma &
        ADSL.adslAturPerfCurr15MinLprs'Image              & Comma &
        ADSL.adslAturPerfCurr15MinESs'Image               & Comma &
        ADSL.adslAturPerfCurr1DayLofs'Image               & Comma &
        ADSL.adslAturPerfCurr1DayLoss'Image               & Comma &
        ADSL.adslAturPerfCurr1DayLprs'Image               & Comma &
        ADSL.adslAturPerfCurr1DayESs'Image                & Comma &
        ADSL.adslAtucChanCorrectedBlks'Image              & Comma &
        ADSL.adslAtucChanUncorrectBlks'Image              & Comma &
        ADSL.adslAtucChanPerfCurr15MinCorrectedBlks'Image & Comma &
        ADSL.adslAtucChanPerfCurr15MinUncorrectBlks'Image & Comma &
        ADSL.adslAtucChanPerfCurr1DayCorrectedBlks'Image  & Comma &
        ADSL.adslAtucChanPerfCurr1DayUncorrectBlks'Image  & Comma &
        ADSL.adslAturChanCorrectedBlks'Image              & Comma &
        ADSL.adslAturChanUncorrectBlks'Image              & Comma &
        ADSL.adslAturChanPerfCurr15MinCorrectedBlks'Image & Comma &
        ADSL.adslAturChanPerfCurr15MinUncorrectBlks'Image & Comma &
        ADSL.adslAturChanPerfCurr1DayCorrectedBlks'Image  & Comma &
        ADSL.adslAturChanPerfCurr1DayUncorrectBlks'Image  & ")";
      return SQL;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         return Ada.Strings.Unbounded.To_Unbounded_String ("UNKNOWN");
   end ADSL_SQL;

   function Int_SQL (TS : Interfaces.C.long; Intf : Structures.Int_Face_Type) return Ada.Strings.Unbounded.Unbounded_String is
   begin
      SQL := Ada.Strings.Unbounded.To_Unbounded_String ("INSERT INTO counters ("                                                                                        &
                                                          "ts,ifIndex,ifAdminStatus,ifOperStatus,ifLastChange,ifInOctets,ifOutOctets,"                                   &
                                                          "ifInDiscards,ifInErrors,ifInUnknownProtos,ifOutDiscards,ifOutErrors,ifInMulticastPkts,ifInBroadcastPkts,"    &
                                                          "ifOutMulticastPkts,ifOutBroadcastPkts,ifHCInOctets,ifHCInUcastPkts,ifHCInMulticastPkts,ifHCInBroadcastPkts," &
                                                          "ifHCOutOctets,ifHCOutUcastPkts,ifHCOutMulticastPkts,ifHCOutBroadcastPkts,ifCounterDiscontinuityTime"         &
                                                          ") values (");
      SQL := SQL & TS'Image             & Comma &
        Intf.ifIndex'Image              & Comma &
        Intf.ifAdminStatus'Image        & Comma &
        Intf.ifOperStatus'Image         & Comma &
        Intf.ifLastChange'Image         & Comma &
        Intf.ifInOctets'Image           & Comma &
        Intf.ifOutOctets'Image          & Comma &
        Intf.ifInDiscards'Image         & Comma &
        Intf.ifInErrors'Image           & Comma &
        Intf.ifInUnknownProtos'Image    & Comma &
        Intf.ifOutDiscards'Image        & Comma &
        Intf.ifOutErrors'Image          & Comma &
        Intf.ifInMulticastPkts'Image    & Comma &
        Intf.ifInBroadcastPkts'Image    & Comma &
        Intf.ifOutMulticastPkts'Image   & Comma &
        Intf.ifOutBroadcastPkts'Image   & Comma &
        Intf.ifHCInOctets'Image         & Comma &
        Intf.ifHCInUcastPkts'Image      & Comma &
        Intf.ifHCInMulticastPkts'Image  & Comma &
        Intf.ifHCInBroadcastPkts'Image  & Comma &
        Intf.ifHCOutOctets'Image        & Comma &
        Intf.ifHCOutUcastPkts'Image     & Comma &
        Intf.ifHCOutMulticastPkts'Image & Comma &
        Intf.ifHCOutBroadcastPkts'Image & Comma &
        Intf.ifCounterDiscontinuityTime'Image & ")";
      return SQL;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " &
                                          GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         return Ada.Strings.Unbounded.To_Unbounded_String ("UNKNOWN");
   end Int_SQL;
end SQL_Helpers;
