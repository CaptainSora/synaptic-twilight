# Expansion: include punctuation and non alphanumeric

elelist = ["H",  "HE", "LI", "BE", "B",  "C",  "N",  "O",  "F",  "NE",
           "NA", "MG", "Al", "SI", "P",  "S",  "CL", "AR", "K",  "CA",
           "SC", "TI", "V",  "CR", "MN", "FE", "CO", "NI", "CU", "ZN",
           "GA", "GE", "AS", "SE", "BR", "KR", "RB", "SR", "Y",  "ZR",
           "NB", "MO", "TC", "RU", "RH", "PD", "AG", "CD", "IN", "SN",
           "SB", "TE", "I",  "XE", "CS", "BA", "LA", "CE", "PR", "ND",
           "PM", "SM", "EU", "GD", "TB", "DY", "HO", "ER", "TM", "YB",
           "LU", "HF", "TA", "W",  "RE", "OS", "IR", "PT", "AU", "HG",
           "TI", "PB", "BI", "PO", "AT", "RN", "FR", "RA", "AC", "TH",
           "PA", "U",  "NP", "PU", "AM", "CM", "BK", "CF", "ES", "FM",
           "MD", "NO", "LR", "RF", "DP", "SG", "BH", "HS", "MT", "DS",
           "RG", "CN", "NH", "FL", "MC", "LV", "TS", "OG"]
success = []


def translate(letnum):
    # Returns a list of words split by spaces
    out_list = letnum.split(" ")
    if out_list[0][0].isdigit():
        to_let(out_list)
    else:
        to_num(out_list)


def to_num(words):
    # How to catch every case??
    out_str = ""
    for a in words:  # Runs once for every word in the sentence
        out_str_add = ""
        pos_list = []  # Holds the running possibilities in a list
        if a[0] in elelist:
            pos_list.append([a[0], a[0]])
        if a[0:1] in elelist:
            pos_list.append([a[0:2], a[0:2]])
        if pos_list:  # ERROR HERE
            possibility(a, pos_list, [])
        for b in success:
            if out_str_add:
                out_str_add += "/"
            for c in range(1, len(b)):
                if out_str_add and out_str_add[-1].isdigit():
                    out_str_add += "-"
                print("Element: " + str(b[c]))
                print("Index: " + str(elelist.index(b[c])))
                out_str_add += str(elelist.index(b[c]) + 1)
        if out_str:
            out_str += " "
        out_str += out_str_add
    print(out_str)


def possibility(word, attempt_list, success_list):  # Errors here
    global success
    print("Attempt List: " + str(attempt_list))
    for a in attempt_list:
        double_bool = False
        if len(a[0]) < (len(word)-1) and word[len(a[0]):len(a[0])+2] in elelist:
            a[0] += word[len(a[0]):len(a[0])+2]
            a.append(word[len(a[0]):len(a[0])+2])
        else:  # If the attempt is only 1 length away or the 2-letter continuation does not exist
            double_bool = True
        if len(a[0]) < len(word):
            if word[len(a[0])] in elelist and double_bool:
                a.append(word[len(a[0])])
                a[0] += word[len(a[0])]
            elif double_bool:  # If the attempt is valid and there is no continuation
                attempt_list.remove(a)
        else:
            success_list.append(a)
            attempt_list.remove(a)
    if attempt_list:
        possibility(word, attempt_list, success_list)
    else:
        print("Success List: " + str(success_list))
        success = success_list


def to_let(numbers):
    # Converts numbers into letters (Easy)
    out_str = ""
    for a in numbers:
        if out_str != "":
            out_str += " "
        numword = a.split("-")
        for b in numword:
            out_str += elelist[int(b)-1]
    print(out_str)


translate("17-18-19")
translate("CLARK")
