"""Module solves task 12 of project Euler""" 
from math import sqrt, floor, ceil

def solution(n):
    """Return minimal triangular number with n dividers"""
    counter = 0
    current = 0

    while True:
        counter+=1
        current+=counter

        ans = 0
        for i in range(1, int(sqrt(current))+1):
            if current%i == 0:
                ans+=2

        if floor(sqrt(current)) == ceil(sqrt(current)):
            ans+=1

        if ans>=n:
            return current

print(solution(30))
