num_set = [-42, 48, 28, -3, -27, 10, 88, 47, -49, -66, 52,
           2, -39, 76, 65, 61, -41, 18, -60, 3, 80, 17,
           -57, 10, 19, -49, 71, 36, 56, -26, -33, -52,
           -96, 17, -54, 63, 55, -38, 17, -37, -48, 85,
           40, -70, 100, 18, 69, 0, 40, -93, 56, 97, -52,
           -69, 95, 89, -73, 86, 57, -79, 61, 50, -17, -62]
success = []
goal = 379
limit = 50  # Soft limit


def not_in(subset):
    # This function returns num_set with subset and everything smaller than subset[-1] removed.
    global num_set
    list1 = list(num_set)
    list1 = list1[list1.index(max(subset))+1:]
    return list1


def sort_set():
    # This function sorts the set and creates a duplicate list of lists for each value.
    global num_set, success
    num_set.sort()
    num_list = []
    print("Searching subsets of length 1")
    for b in num_set:
        num_list.append([b])
        if b == goal:
            success.append([b])
            print([b])
    print(str(len(success)) + " successful subsets of length 1")
    find_sum(num_list, 0)


def find_sum(lt_set, success_counter):
    # This recursive function takes a list of lists and distributes every unused value from the original
    # set to create a new list.
    global success, goal, limit
    lt_goal = []  # List of lists that sum to less than the goal
    temp_counter = 0
    print("Searching subsets of length " + str(len(lt_set[0]) + 1))
    for a in lt_set:
        other_set = not_in(a)
        for b in other_set:
            temp = a + [b]
            if sum(temp) == goal and add_success(temp):
                success.append(temp)
                print(temp)
                temp_counter += 1
            elif sum(temp) < goal:
                lt_goal.append(temp)
    if len(lt_goal) > 0:
        print(str(temp_counter) + " successful subsets of length " + str(len(lt_goal[0])))
    if len(lt_goal) > 0 and len(lt_goal[-1]) < len(num_set) and len(success) < limit:
        # If there are still sets that sum to less than the goal and the last set is shorter than the original
        # set, and the length of successes is less than 100, then call the function again.
        find_sum(lt_goal, success_counter + temp_counter)
    elif len(success) > 0:
        # If either the list of less than goals is exhausted or the last set is the same length as the original
        # (i.e. nothing more to add) then print successes if they exist
        print("First " + str(len(success)) + " successful subsets shown.")
    else:
        # If the above conditional is true but there are no successful subsets, then print the failure
        print("No results.")


def add_success(newlist):
    # Checks if an iteration of the subset already exists.
    # GOOD PLACE TO OPTIMIZE
    global success
    newlist.sort()
    for a in success:
        if a == newlist:
            return False
    return True


sort_set()
