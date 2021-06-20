#!/usr/bin/python

import time


def get_data():
    with open('/proc/stat') as f:
        l = f.readline()
        l = [int(x) for x in l.split()[1:]]
    return l


def get_idle(d):
    return d[3] + d[4]


def get_non_idle(d):
    return d[0] + d[1] + d[2] + d[5] + d[6] + d[7]


def get_usage():
    prev = get_data()
    time.sleep(0.3)
    curr = get_data()
    prev_idle = get_idle(prev)
    prev_non_idle = get_non_idle(prev)
    curr_idle = get_idle(curr)
    curr_non_idle = get_non_idle(curr)

    prevtot = prev_idle + prev_non_idle
    currtot = curr_idle + curr_non_idle
    totald = currtot - prevtot
    idled = curr_idle - prev_idle

    p = (totald - idled) / totald
    return round(p * 100, 2)


print(get_usage())
