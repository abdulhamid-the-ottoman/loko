;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-FileCopyrightText: none
;; SPDX-License-Identifier: MIT
;; Automatically generated by header-snarfer.scm
;; This file is not a creative work.
#!r6rs

(library (loko net numbers)
  (export
    sizeof-ether_header
    offsetof-ether_header-ether_dhost
    offsetof-ether_header-ether_shost
    offsetof-ether_header-ether_type
    ETH_ALEN
    sizeof-ether_arp
    offsetof-ether_arp-arp_hrd
    offsetof-ether_arp-arp_pro
    offsetof-ether_arp-arp_hln
    offsetof-ether_arp-arp_pln
    offsetof-ether_arp-arp_op
    offsetof-ether_arp-arp_sha
    offsetof-ether_arp-arp_spa
    offsetof-ether_arp-arp_tha
    offsetof-ether_arp-arp_tpa
    ARPHRD_NETROM
    ARPHRD_ETHER
    ARPHRD_AX25
    ARPHRD_CHAOS
    ARPOP_REQUEST
    ARPOP_REPLY
    ETHERTYPE_IPV6
    ETHERTYPE_IP
    ETHERTYPE_ARP
    ETHERTYPE_VLAN
    sizeof-ip6_hdr
    offsetof-ip6_hdr-ip6_vfc
    offsetof-ip6_hdr-ip6_flow
    offsetof-ip6_hdr-ip6_plen
    offsetof-ip6_hdr-ip6_nxt
    offsetof-ip6_hdr-ip6_hlim
    offsetof-ip6_hdr-ip6_src
    offsetof-ip6_hdr-ip6_dst
    sizeof-ip6_ext
    offsetof-ip6_ext-ip6e_nxt
    offsetof-ip6_ext-ip6e_len
    sizeof-ip6_opt
    offsetof-ip6_opt-ip6o_type
    offsetof-ip6_opt-ip6o_len
    sizeof-iphdr
    offsetof-iphdr-tos
    offsetof-iphdr-tot_len
    offsetof-iphdr-id
    offsetof-iphdr-frag_off
    offsetof-iphdr-ttl
    offsetof-iphdr-protocol
    offsetof-iphdr-check
    offsetof-iphdr-saddr
    offsetof-iphdr-daddr
    IPPROTO_ICMP
    IPPROTO_ICMPV6
    IPPROTO_NONE
    IPPROTO_SCTP
    IPPROTO_TCP
    IPPROTO_UDP
    sizeof-udphdr
    offsetof-udphdr-uh_sport
    offsetof-udphdr-uh_dport
    offsetof-udphdr-uh_ulen
    offsetof-udphdr-uh_sum
    sizeof-icmp6_hdr
    offsetof-icmp6_hdr-icmp6_type
    offsetof-icmp6_hdr-icmp6_code
    offsetof-icmp6_hdr-icmp6_cksum
    offsetof-icmp6_hdr-icmp6_data32
    ICMP6_ECHO_REQUEST
    ICMP6_ECHO_REPLY
    MLD_LISTENER_QUERY
    MLD_LISTENER_REPORT
    MLD_LISTENER_REDUCTION
    ND_ROUTER_SOLICIT
    ND_ROUTER_ADVERT
    ND_NEIGHBOR_SOLICIT
    ND_NEIGHBOR_ADVERT
    ND_REDIRECT
    ICMP6_ROUTER_RENUMBERING
    sizeof-nd_opt_hdr
    offsetof-nd_opt_hdr-nd_opt_type
    offsetof-nd_opt_hdr-nd_opt_len
    ND_OPT_SOURCE_LINKADDR
    ND_OPT_TARGET_LINKADDR
    ND_OPT_PREFIX_INFORMATION
    ND_OPT_REDIRECTED_HEADER
    ND_OPT_MTU
    ND_OPT_RTR_ADV_INTERVAL
    ND_OPT_HOME_AGENT_INFO
    sizeof-icmphdr
    offsetof-icmphdr-type
    offsetof-icmphdr-code
    offsetof-icmphdr-checksum
    ICMP_ECHOREPLY
    ICMP_DEST_UNREACH
    ICMP_SOURCE_QUENCH
    ICMP_REDIRECT
    ICMP_ECHO
    ICMP_TIME_EXCEEDED
    ICMP_PARAMETERPROB
    ICMP_TIMESTAMP
    ICMP_TIMESTAMPREPLY
    ICMP_INFO_REQUEST
    ICMP_INFO_REPLY
    ICMP_ADDRESS
    ICMP_ADDRESSREPLY
    ICMP_NET_UNREACH
    ICMP_HOST_UNREACH
    ICMP_PROT_UNREACH
    ICMP_PORT_UNREACH
    ICMP_FRAG_NEEDED
    ICMP_SR_FAILED
    ICMP_NET_UNKNOWN
    ICMP_HOST_UNKNOWN
    ICMP_HOST_ISOLATED
    ICMP_NET_ANO
    ICMP_HOST_ANO
    ICMP_NET_UNR_TOS
    ICMP_HOST_UNR_TOS
    ICMP_PKT_FILTERED
    ICMP_PREC_VIOLATION
    ICMP_PREC_CUTOFF
    ICMP_EXC_TTL
    ICMP_EXC_FRAGTIME
    sizeof-tcphdr
    offsetof-tcphdr-th_sport
    offsetof-tcphdr-th_dport
    offsetof-tcphdr-th_seq
    offsetof-tcphdr-th_ack
    offsetof-tcphdr-th_flags
    offsetof-tcphdr-th_win
    offsetof-tcphdr-th_sum
    offsetof-tcphdr-th_urp
    offsetof-tcphdr-th_off
    TH_FIN
    TH_SYN
    TH_RST
    TH_PUSH
    TH_ACK
    TH_URG)
  (import (rnrs (6)))

(define-syntax define-inlined
  (syntax-rules ()
    ((_ name v)
     (define-syntax name (identifier-syntax v)))))

;;; netinet/ether.h
(define-inlined sizeof-ether_header 14)
(define-inlined offsetof-ether_header-ether_dhost 0)
(define-inlined offsetof-ether_header-ether_shost 6)
(define-inlined offsetof-ether_header-ether_type 12)
(define-inlined ETH_ALEN 6)
(define-inlined sizeof-ether_arp 28)
(define-inlined offsetof-ether_arp-arp_hrd 0)
(define-inlined offsetof-ether_arp-arp_pro 2)
(define-inlined offsetof-ether_arp-arp_hln 4)
(define-inlined offsetof-ether_arp-arp_pln 5)
(define-inlined offsetof-ether_arp-arp_op 6)
(define-inlined offsetof-ether_arp-arp_sha 8)
(define-inlined offsetof-ether_arp-arp_spa 14)
(define-inlined offsetof-ether_arp-arp_tha 18)
(define-inlined offsetof-ether_arp-arp_tpa 24)
(define-inlined ARPHRD_NETROM 0)
(define-inlined ARPHRD_ETHER 1)
(define-inlined ARPHRD_AX25 3)
(define-inlined ARPHRD_CHAOS 5)
(define-inlined ARPOP_REQUEST 1)
(define-inlined ARPOP_REPLY 2)
(define-inlined ETHERTYPE_IPV6 #x86dd)
(define-inlined ETHERTYPE_IP #x0800)
(define-inlined ETHERTYPE_ARP #x0806)
(define-inlined ETHERTYPE_VLAN #x8100)

;;; netinet/ip6.h
(define-inlined sizeof-ip6_hdr 40)
(define-inlined offsetof-ip6_hdr-ip6_vfc 0)
(define-inlined offsetof-ip6_hdr-ip6_flow 0)
(define-inlined offsetof-ip6_hdr-ip6_plen 4)
(define-inlined offsetof-ip6_hdr-ip6_nxt 6)
(define-inlined offsetof-ip6_hdr-ip6_hlim 7)
(define-inlined offsetof-ip6_hdr-ip6_src 8)
(define-inlined offsetof-ip6_hdr-ip6_dst 24)
(define-inlined sizeof-ip6_ext 2)
(define-inlined offsetof-ip6_ext-ip6e_nxt 0)
(define-inlined offsetof-ip6_ext-ip6e_len 1)
(define-inlined sizeof-ip6_opt 2)
(define-inlined offsetof-ip6_opt-ip6o_type 0)
(define-inlined offsetof-ip6_opt-ip6o_len 1)

;;; netinet/ip.h
(define-inlined sizeof-iphdr 20)
(define-inlined offsetof-iphdr-tos 1)
(define-inlined offsetof-iphdr-tot_len 2)
(define-inlined offsetof-iphdr-id 4)
(define-inlined offsetof-iphdr-frag_off 6)
(define-inlined offsetof-iphdr-ttl 8)
(define-inlined offsetof-iphdr-protocol 9)
(define-inlined offsetof-iphdr-check 10)
(define-inlined offsetof-iphdr-saddr 12)
(define-inlined offsetof-iphdr-daddr 16)

;;; netinet/in.h
(define-inlined IPPROTO_ICMP 1)
(define-inlined IPPROTO_ICMPV6 58)
(define-inlined IPPROTO_NONE 59)
(define-inlined IPPROTO_SCTP 132)
(define-inlined IPPROTO_TCP 6)
(define-inlined IPPROTO_UDP 17)

;;; netinet/udp.h
(define-inlined sizeof-udphdr 8)
(define-inlined offsetof-udphdr-uh_sport 0)
(define-inlined offsetof-udphdr-uh_dport 2)
(define-inlined offsetof-udphdr-uh_ulen 4)
(define-inlined offsetof-udphdr-uh_sum 6)

;;; netinet/icmp6.h
(define-inlined sizeof-icmp6_hdr 8)
(define-inlined offsetof-icmp6_hdr-icmp6_type 0)
(define-inlined offsetof-icmp6_hdr-icmp6_code 1)
(define-inlined offsetof-icmp6_hdr-icmp6_cksum 2)
(define-inlined offsetof-icmp6_hdr-icmp6_data32 4)
(define-inlined ICMP6_ECHO_REQUEST 128)
(define-inlined ICMP6_ECHO_REPLY 129)
(define-inlined MLD_LISTENER_QUERY 130)
(define-inlined MLD_LISTENER_REPORT 131)
(define-inlined MLD_LISTENER_REDUCTION 132)
(define-inlined ND_ROUTER_SOLICIT 133)
(define-inlined ND_ROUTER_ADVERT 134)
(define-inlined ND_NEIGHBOR_SOLICIT 135)
(define-inlined ND_NEIGHBOR_ADVERT 136)
(define-inlined ND_REDIRECT 137)
(define-inlined ICMP6_ROUTER_RENUMBERING 138)
(define-inlined sizeof-nd_opt_hdr 2)
(define-inlined offsetof-nd_opt_hdr-nd_opt_type 0)
(define-inlined offsetof-nd_opt_hdr-nd_opt_len 1)
(define-inlined ND_OPT_SOURCE_LINKADDR 1)
(define-inlined ND_OPT_TARGET_LINKADDR 2)
(define-inlined ND_OPT_PREFIX_INFORMATION 3)
(define-inlined ND_OPT_REDIRECTED_HEADER 4)
(define-inlined ND_OPT_MTU 5)
(define-inlined ND_OPT_RTR_ADV_INTERVAL 7)
(define-inlined ND_OPT_HOME_AGENT_INFO 8)

;;; netinet/ip_icmp.h
(define-inlined sizeof-icmphdr 8)
(define-inlined offsetof-icmphdr-type 0)
(define-inlined offsetof-icmphdr-code 1)
(define-inlined offsetof-icmphdr-checksum 2)
(define-inlined ICMP_ECHOREPLY 0)
(define-inlined ICMP_DEST_UNREACH 3)
(define-inlined ICMP_SOURCE_QUENCH 4)
(define-inlined ICMP_REDIRECT 5)
(define-inlined ICMP_ECHO 8)
(define-inlined ICMP_TIME_EXCEEDED 11)
(define-inlined ICMP_PARAMETERPROB 12)
(define-inlined ICMP_TIMESTAMP 13)
(define-inlined ICMP_TIMESTAMPREPLY 14)
(define-inlined ICMP_INFO_REQUEST 15)
(define-inlined ICMP_INFO_REPLY 16)
(define-inlined ICMP_ADDRESS 17)
(define-inlined ICMP_ADDRESSREPLY 18)
(define-inlined ICMP_NET_UNREACH 0)
(define-inlined ICMP_HOST_UNREACH 1)
(define-inlined ICMP_PROT_UNREACH 2)
(define-inlined ICMP_PORT_UNREACH 3)
(define-inlined ICMP_FRAG_NEEDED 4)
(define-inlined ICMP_SR_FAILED 5)
(define-inlined ICMP_NET_UNKNOWN 6)
(define-inlined ICMP_HOST_UNKNOWN 7)
(define-inlined ICMP_HOST_ISOLATED 8)
(define-inlined ICMP_NET_ANO 9)
(define-inlined ICMP_HOST_ANO 10)
(define-inlined ICMP_NET_UNR_TOS 11)
(define-inlined ICMP_HOST_UNR_TOS 12)
(define-inlined ICMP_PKT_FILTERED 13)
(define-inlined ICMP_PREC_VIOLATION 14)
(define-inlined ICMP_PREC_CUTOFF 15)
(define-inlined ICMP_EXC_TTL 0)
(define-inlined ICMP_EXC_FRAGTIME 1)

;;; netinet/tcp.h
(define-inlined sizeof-tcphdr 20)
(define-inlined offsetof-tcphdr-th_sport 0)
(define-inlined offsetof-tcphdr-th_dport 2)
(define-inlined offsetof-tcphdr-th_seq 4)
(define-inlined offsetof-tcphdr-th_ack 8)
(define-inlined offsetof-tcphdr-th_flags 13)
(define-inlined offsetof-tcphdr-th_win 14)
(define-inlined offsetof-tcphdr-th_sum 16)
(define-inlined offsetof-tcphdr-th_urp 18)
(define-inlined offsetof-tcphdr-th_off 12)
(define-inlined TH_FIN #x01)
(define-inlined TH_SYN #x02)
(define-inlined TH_RST #x04)
(define-inlined TH_PUSH #x08)
(define-inlined TH_ACK #x10)
(define-inlined TH_URG #x20)
)
