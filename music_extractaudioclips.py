'''
Usage: clipFilesFromSpreadSheet(filename)

where the file is a text file with header:
file_name	start_time	end_time	output_suffix
filename.mp4    hh:mm:ss.zzz    ss.zzz  cool clip.mp4

(hh:mm:ss.zzz, mm:ss.zzz, and ss.zzz are all valid formats. zzz means milliseconds.)

Put full video or audio files in folder "in", run, and clips will be put in folder "out"; these paths can be edited. Calls FFMPEG to do the clipping.
'''

from subprocess import call

def clipFilesFromSpreadSheet(filename = "audiolist.txt"):
    #filename = "audiolist.txt"
    myfile = readFile(filename)
    myfile = myfile.split("\n")
    myfile = [x.split("\t") for x in myfile]
    myfile = myfile[1:] #exclude header row
    clipdatalines = [readClipData_fromLine(x) for x in myfile]
    for i in range(len(clipdatalines)):
        executeVideoAudioClip(clipdatalines[i])
    return
#
def readFile(myfile):
    try:
        f = open(myfile, mode="r", encoding="utf-8")
    except:
        f = open(myfile, mode="r")
    content = f.read()
    f.close()
    return content
def readClipData_fromLine(myline):
    input_video_title = myline[0]
    clip_starttime_asstring = hhmmss_toSeconds(myline[1])
    clip_endtime_asstring = hhmmss_toSeconds(myline[2])
    output_name_suffix = myline[3]
    return [input_video_title, clip_starttime_asstring, clip_endtime_asstring, output_name_suffix]
def hhmmss_toSeconds(mytimestamp):
    #valid input formats: like hh:mm:ss.zzz, mm:ss.zzz, ss, ss.zzz
    #get milliseconds
    mytimestamp_seconds = 0.
    if len(mytimestamp.split(".")) > 2:
        print("more than two periods in timestamp. that's a weird error case.")
        return None
    elif len(mytimestamp.split(".")) > 1:
        mytimestamp_seconds += float('0.'+mytimestamp.split(".")[1])
        mytimestamp = mytimestamp.split(".")[0]
    #get hours, minutes, seconds
    splitstamp = mytimestamp.split(":")
    if len(splitstamp) > 3:
        print("more than two colons in timestamp. that's a weird error case.")
        return None
    elif len(splitstamp) == 3:
        mytimestamp_seconds += 3600*float(splitstamp[0]) + 60*float(splitstamp[1]) + float(splitstamp[2])
    elif len(splitstamp) == 2:
        mytimestamp_seconds += 60*float(splitstamp[0]) + float(splitstamp[1])
    elif len(splitstamp) == 1:
        mytimestamp_seconds += float(splitstamp[0])
    return str(mytimestamp_seconds)
#
def executeVideoAudioClip(clipdata):
    input_video_title, clip_starttime_asstring, clip_endtime_asstring, output_name_suffix = clipdata
    output_extension = output_name_suffix.split(".")[-1]
    input_video_path = "in" + "/" + input_video_title
    output_video_path = "out" + "/" + makeOutputClipName(input_video_title, output_name_suffix, clip_starttime_asstring)
    #audioexts = ["ogg", "mp3", "wav"] #maybe handle these separately.
    audioexts = ["ogg"]
    videoexts = ["mp4"]
    if output_extension in audioexts:
        mycommand = "ffmpeg -i \""+(input_video_path)+"\" -ss " + (getHybridSeekTime(clip_starttime_asstring)) + " -i \""+(input_video_path)+"\" -ss " + (getAccurateStartSeekTime(clip_starttime_asstring)) + " -t " + (getClipDuration(clip_starttime_asstring, clip_endtime_asstring)) + " "+"-map 0:0 -map 1:1? -c:a libvorbis -qscale:v 7"+" \""+(output_video_path)+"\""
            #uses two inputs to try and get a cover image for audio files?... I forgot.
    elif output_extension in videoexts:
        mycommand = "ffmpeg -ss " + (getHybridSeekTime(clip_starttime_asstring)) + " -i \""+(input_video_path)+"\" -ss " + (getAccurateStartSeekTime(clip_starttime_asstring)) + " -t " + (getClipDuration(clip_starttime_asstring, clip_endtime_asstring)) + " "+"-c:v libx264 -c:a copy"+" \""+(output_video_path)+"\""
    else:
        print("use mp4 or ogg extension for the clip name.")
        return None
    call(mycommand, shell = True)
    return
def getHybridSeekTime(starttime):
    if float(starttime) < 60:
        return '0'
    else:
        return str(float(starttime) - 60)
def getAccurateStartSeekTime(starttime):
    if float(starttime) < 60:
        return starttime
    else:
        return '60'
def getClipDuration(starttime, endtime):
    return str(round(float(endtime) - float(starttime), 3))
def makeOutputClipName(input_video_title, output_name_suffix, clip_starttime_asstring):
    splitstamp = clip_starttime_asstring.split(":")
    if len(splitstamp) > 3:
        print("more than two colons in timestamp. that's a weird error case.")
        return None
    elif len(splitstamp) == 3:
        timestamp_thing = splitstamp[0]+"h"+splitstamp[1]+"m"+splitstamp[2]+"s"
    elif len(splitstamp) == 2:
        timestamp_thing = splitstamp[0]+"m"+splitstamp[1]+"s"
    elif len(splitstamp) == 1:
        alltime = float(splitstamp[0])
        if alltime < 60:
            timestamp_thing = str(alltime)+"s"
        else: #let's format this so it's human-readable
            timestamp_thing = nicelyFormatSeconds_tohhmmss(alltime)
    return input_video_title + " " + timestamp_thing + " " + output_name_suffix
def nicelyFormatSeconds_tohhmmss(secondsonly_time_asfloat):
    hours = int(secondsonly_time_asfloat // 3600)
    minutes = int((secondsonly_time_asfloat - 3600*hours)//60)
    seconds = secondsonly_time_asfloat - 3600*hours - 60*minutes
    seconds_whole = int(seconds)
    milliseconds = round(seconds - seconds_whole, 3)
    timestamp_string = ""
    if hours > 0:
        timestamp_string += str(hours).zfill(2) + "h"
    if not(hours == 0 and minutes == 0):
        timestamp_string += str(minutes).zfill(2) + "m"
    timestamp_string += str(seconds_whole).zfill(2)
    if milliseconds != 0:
        timestamp_string += "." + str(milliseconds)[2:].ljust(3,'0')
    timestamp_string += "s"
    return timestamp_string

if __name__ == "__main__":
    clipFilesFromSpreadSheet()
