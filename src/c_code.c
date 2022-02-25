/*
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
*/
#include <stdio.h>
#include <inttypes.h>
#include <net-snmp/net-snmp-config.h>
#include <net-snmp/net-snmp-includes.h>

netsnmp_pdu *tmp_pdu;

int local_generate_Ku (netsnmp_session *s, char *pass) {
  return generate_Ku (s->securityAuthProto, s->securityAuthProtoLen, (const u_char *) pass, strlen(pass), s->securityAuthKey, &s->securityAuthKeyLen);
};

void local_snmp_synch_response(netsnmp_session *s, netsnmp_pdu *p, netsnmp_pdu *np, int *Res) {
  if (tmp_pdu) {
    snmp_free_pdu (tmp_pdu);
  }

  if ((*Res = snmp_synch_response(s, p, &tmp_pdu)) != 0) {
    NULL;
  }

  memcpy (np, tmp_pdu, sizeof (*tmp_pdu));
};
