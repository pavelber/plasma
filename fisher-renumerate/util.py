from enum import Enum




class ConversionType(Enum):
    ADD_100_TO_200 = 1
    NEGATIVE_TO_200 = 2


def process_line(line, start, end, conversion_type: ConversionType):
    try:
        # Extract the number from positions 38-41
        num_str = line[start:end]
        num = int(num_str)
        len = end - start
        match conversion_type:
            case ConversionType.ADD_100_TO_200:
                if 200 <= num <= 299:
                    num += 100
            case ConversionType.NEGATIVE_TO_200:
                if num < 0:
                    num = 200 - num
            case _:
                raise Exception("Unknown conversion type")

        # Replace the original number with the modified one
        modified_line = line[:start] + str(num).rjust(len) + line[end:]
        return modified_line
    except ValueError:
        # If conversion to integer fails, return the original line
        return line
