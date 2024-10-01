"""Module solves task 19 of project Euler"""

def days(year, month):
    """Return amount of days in 'year : month'"""
    if month in [4, 6, 9, 11]:
        return 30
    if month == 2:
        if year % 4 == 0 and year % 100 != 0 or year % 400 == 0:
            return 29
        return 28
    return 31



def solution(year_id, month_id, day_id):
    """Return amount of sundays"""
    sun_first = 0
    while (year_id <= 2000 and month_id <= 12 and day_id <= 31):
        if(day_id == 1 and year_id > 1900):
            sun_first+=1
        day_id+=7
        days_month = days(year_id, month_id)

        if day_id > days_month:
            day_id -= days_month
            month_id+=1

            if month_id > 12:
                month_id = 1
                year_id+=1

    return sun_first

print(solution(1901, 1, 1))
