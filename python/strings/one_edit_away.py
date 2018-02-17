def one_edit_away(a, b):
    if abs(len(a) - len(b)) > 1:
        return False

    shorter, longer = a, b if len(a) < len(b) else b, a
    idx1 = 0  # Longer string
    idx2 = 0  # Shorter string
    different = False
    while idx1 < len(longer) and idx2 < len(shorter):
        if shorter[idx1] != longer[idx2]:
            if different:  # Already spent our edit budget
                return True
            different = True

            # If lengths are equal, then we have to try replacing
            if len(longer) == len(shorter):
                idx2 += 1
            # Else, we only advance the longer string, thus we have an
            # "insertion"
        else:
            idx2 += 1  # Characters match; keep going
        idx1 += 1  # Always advance the longer string

    return True
