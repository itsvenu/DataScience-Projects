
'''
python AutoZOOM_v0.03.py --zoom_link "zoom-meeting-link" --meeting_date "06-09-2020" --meeting_time "13-38-00"
'''

# required modules
import pyautogui as pyg
import webbrowser as wb
import datetime
import time
import click

# functions to format date, time
def format_date(x):
    date_list = x.split(sep="-")
    return list(map(int, date_list))

def format_time(x):
    time_list = x.split(sep="-")
    return list(map(int, time_list))

def given_datetime(given_date, given_time):

    # YY, MM, DD, HH, MM
    return datetime.datetime(given_date[2], given_date[1], given_date[0], given_time[0], given_time[1], given_time[2])

# join the meeting
def join_meeting(zoom_link, meeting_date, meeting_time):

    meeting_date_x = format_date(meeting_date)
    meeting_time_x = format_time(meeting_time)
    required_datetime = given_datetime(meeting_date_x, meeting_time_x)

    # time difference between current and meeting time
    wait_time_sec = (required_datetime - datetime.datetime.now().replace(microsecond=0)).total_seconds()
    print("Your ZOOM meeting starts in" + str(wait_time_sec/60) + " min")
    time.sleep(wait_time_sec)

    # zoom app related
    wb.get(using='chrome').open(zoom_link, new=2) #open zoom link in a new window
    time.sleep(5) # given time for the link to show app top-up window
    pyg.click(x=805, y=254, clicks=1, interval=0, button='left') # click on open zoom.app option
    time.sleep(10) # wait for 10 sec
    pyg.click(x=195, y=31, clicks=1, interval=0, button='left') # maximize zoom app
    time.sleep(3) # wait for 3 sec
    pyg.click(x=50, y=776, clicks=1, interval=0, button='left') # mute yourself

@click.command()
@click.option('--zoom_link',
              help="full ZOOM meeting link",
              required=True)

@click.option('--meeting_date',
              help="date of the meeting in the format DD-MM-YYYY",
              required=True)

@click.option('--meeting_time',
              help="time of the meeting in the format HH-MM-SS",
              required=True)

##
def zoom_meeting(zoom_link, meeting_date, meeting_time):
    join_meeting(zoom_link, meeting_date, meeting_time)

if __name__ == '__main__':
    zoom_meeting()
