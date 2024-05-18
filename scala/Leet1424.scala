import scala.util.chaining._
import scala.collection.mutable

object Leet1424 extends App {

  object Solution {
    val cache = mutable.Map.empty[(Int, Int), Int]

    def maxScore(cardPoints: Array[Int], k: Int): Int = {
      cache.clear
      maxScoreBetween(cardPoints, 0, cardPoints.length - 1, k)
    }

    def maxScoreBetween(cardPoints: Array[Int], from: Int, to: Int, k: Int): Int = {
      cache.get((from, to)) match {
        case Some(cachedValue) => cachedValue
        case _ =>
          (k match {
            case 0 => 0
            case k =>
              (cardPoints(from) + maxScoreBetween(cardPoints, from + 1, to, k - 1)) max
                (cardPoints(to) + maxScoreBetween(cardPoints, from, to - 1, k - 1))
          }).tap(v => cache += ((from, to) -> v))
      }
    }
  }

  println(Solution.maxScore(Array(173, 710, 930, 125, 549, 480, 799, 962, 304, 543, 434, 625, 957, 933, 160, 912, 303, 326, 581, 268, 863, 830, 380, 229, 890, 654, 658, 955, 111, 264, 199, 768, 197, 545, 818, 549, 757, 76, 938, 85, 367, 268, 599, 953, 986, 556, 903, 836, 865, 706, 989, 16, 366, 536, 675, 770, 21, 853, 739, 257, 986, 588, 934, 571, 217, 633, 809, 357, 441, 504, 131, 510, 388, 750, 99, 82, 699, 527, 452, 592, 427, 84, 286, 354, 984, 175, 922, 348, 71, 24, 473, 809, 421, 578, 431, 518, 819, 547, 181, 671, 1000, 627, 787, 65, 721, 968, 777, 577, 45, 740, 142, 317, 878, 688, 692, 462, 60, 927, 820, 130, 360, 789, 744, 983, 861, 17, 547, 453, 294, 413, 422, 540, 304, 347, 583, 896, 30, 650, 853, 849, 688, 958, 402, 242, 730, 238, 819, 958, 78, 981, 856, 572, 820, 87, 903, 832, 930, 687, 863, 580, 480, 390, 330, 876, 864, 434, 887, 822, 961, 719, 891, 4, 855, 5, 155, 147, 419, 746, 30, 589, 412, 872, 539, 885, 21, 46, 239, 848, 41, 307, 71, 17, 453, 463, 861, 951, 383, 951, 918, 312, 566, 568, 257, 682, 429, 890, 971, 310, 993, 19, 479, 810, 94, 91, 160, 959, 444, 527, 9, 695, 775, 322, 288, 141, 865, 941, 235, 993, 981, 945, 400, 172, 988, 640, 647, 715, 126, 893, 290, 39, 174, 525, 38, 210, 957, 760, 451, 780, 431, 769, 787, 981, 578, 147, 186, 751, 634, 554, 333, 459, 288, 602, 498, 90, 660, 430, 46, 143, 600, 903, 401, 350, 432, 278, 37, 763, 794, 753, 199, 723, 71, 795, 888, 165, 641, 206, 897, 334, 663, 815, 40, 487, 954, 606, 117, 968, 542, 448, 203, 439, 341, 153, 876, 446, 741, 628, 720, 567, 517, 463, 357, 504, 77, 544, 945, 879, 715, 651, 149, 677, 156, 817, 524, 320, 291, 67, 37, 333, 668, 633, 259, 11, 861, 157, 915, 587, 46, 247, 401, 1, 621, 897, 423, 175, 936, 829, 289, 781, 789, 797, 406, 711, 685, 594, 397, 94, 202, 752, 247, 61, 246, 14, 238, 681, 87, 517, 40, 917, 84, 434, 255, 10, 455, 311, 801, 351, 635, 698, 681, 595, 863, 324, 928, 83, 49, 570, 612, 601, 471, 892, 133, 664, 378, 912, 419, 119, 252, 267, 725, 728, 293, 16, 469, 267, 845, 681, 96, 871, 42, 122, 366, 308, 398, 683, 590, 865, 5, 198, 236, 477, 800, 992, 398, 420, 112, 581, 42, 79, 546, 396, 432, 170, 401, 644, 814, 927, 239, 679, 934, 109, 510, 518, 87, 551, 252, 418, 64, 276, 756, 706, 646, 765, 393, 666, 374, 700, 53, 803, 883, 625, 4, 428, 946, 714, 617, 240, 377, 58, 554, 493, 959, 156, 526, 869, 217, 980, 272, 119, 32, 133, 687, 721, 92, 921, 16, 60, 256, 904, 569, 689, 660, 98, 695, 118, 312, 14, 819, 334, 438, 160, 385, 350, 949, 603, 873, 366, 101, 682, 753, 741, 429, 445, 102, 156, 19, 527, 278, 968, 482, 212, 953, 264, 626, 76, 997, 119, 549, 129, 650, 929, 992, 385, 384, 571, 515, 971, 692, 880, 842, 659, 811, 949, 744, 755, 825, 681, 151, 657, 352, 619, 521, 881, 902, 17, 455, 458, 128, 426, 487, 454, 782, 977, 496, 621, 992, 596, 811, 673, 776, 709, 25, 266, 892, 24, 989, 158, 162, 128, 910, 729, 366, 131, 330, 812, 30, 708, 80, 197, 765, 380, 66, 862, 788, 686, 294, 990, 951, 172, 196, 694, 210, 323, 930, 416, 378, 770, 802, 514, 961, 255, 837, 171, 62, 330, 262, 662, 429, 492, 424, 460, 108, 747, 581, 979, 942, 566, 90, 854, 261, 472, 834, 974, 408, 184, 103, 187, 642, 371, 338, 606, 271, 445, 507, 864, 229, 378, 541, 795, 788, 322, 662, 579, 825, 625, 903, 310, 620, 96, 480, 915, 703, 333, 938, 670, 814, 414, 501, 556, 337, 645, 900, 937, 827, 460, 797, 453, 113, 122, 745, 736, 112, 159, 705, 514, 715, 67, 949, 567, 525, 538, 187, 783, 624, 272, 727, 619, 263, 352, 300, 971, 358, 930, 254, 768, 816, 760, 722, 441, 334, 856, 680, 508, 267, 704, 852, 142, 81, 609, 96, 538, 261, 429, 336, 278, 272, 505, 731, 406, 988, 565, 897, 489, 476, 715, 442, 131, 358, 92, 54, 541, 351, 653, 818, 930, 577, 333, 389, 17, 2, 818, 75, 774, 503, 22, 425, 801, 699, 479, 523, 11, 424, 128, 988, 966, 996, 184, 511, 279, 539, 716, 455, 593, 242, 470, 493, 196, 314, 8, 337, 313, 393, 292, 319, 778, 438, 634, 931, 611, 980, 619, 663, 60, 714, 373, 951, 886, 979, 995, 541, 984, 58, 99, 282, 526, 401, 191, 726, 593, 217, 594, 380, 801, 110, 862, 828, 836, 981, 379, 197, 278, 638, 235, 775, 739, 941, 60, 364, 435, 646, 776, 62, 217, 518, 836, 603, 868, 338, 915, 214, 188, 321, 774, 788, 64, 73, 768, 496, 499, 452, 607, 566, 348, 7, 882, 902, 728, 611, 767, 377, 835, 804, 488, 128, 753, 8, 155, 135, 82, 411, 747, 290, 25, 964, 415, 301, 456, 571, 608, 774, 631, 823, 954, 460, 422, 121, 970, 995, 821, 196, 275, 333, 901, 137, 720, 397, 113, 971, 708, 867, 613, 376, 503, 362, 782, 214, 712, 682, 298, 15, 795, 674, 591, 251, 39, 481, 297, 692, 369, 355, 503, 364, 996, 780, 805, 576, 193, 388, 416, 236, 354, 843, 921, 188, 890, 576, 484, 782, 712, 269, 338, 878, 714, 467, 176, 713, 60, 369, 789, 606, 999, 924, 126, 715, 713, 650, 202, 267, 409, 107, 282, 390, 850, 880, 110, 455, 420, 720, 752, 575, 259, 925, 915, 227, 833, 771, 709, 26, 71, 711, 729, 56, 21, 42, 482, 928, 757, 58, 882, 746, 600, 963, 805, 908, 397, 833, 701, 140, 34, 991, 382, 724, 80, 40, 473, 12, 833, 156, 389, 544, 488, 5, 274, 415, 762, 344, 190, 245, 598, 107, 609, 574, 809, 588, 235, 765, 621, 747, 201, 516, 221, 562, 205, 325, 151, 923, 728, 202, 516, 616, 772, 286, 580, 165, 866, 307, 475, 542, 158, 327, 137, 799, 51, 183, 18, 671, 834, 233, 812, 977, 891, 753, 542, 112, 917, 987, 740, 905, 67, 902, 939, 562, 379, 371, 851, 883, 749, 669, 712, 528, 593, 7, 902, 572, 603, 88, 899, 141, 191, 630, 627, 61, 264, 498, 997, 705, 780, 507, 778, 819, 855, 853, 439, 5, 953, 889, 522, 418, 175, 845, 160, 611, 408, 469, 657, 406, 129, 256, 876, 298, 682, 971, 371, 116, 395, 684, 573, 769, 944, 102, 253, 402, 875, 192, 575, 991, 830, 307, 665, 805, 644, 764, 747, 573, 781, 508, 835, 298, 591, 652, 350, 51, 282, 868, 941, 468, 937, 994, 895, 486, 304, 585, 718, 516, 6, 713, 74, 972, 613, 272, 7, 85, 27, 90, 338, 917, 306, 574, 545, 815, 672, 314, 665, 719, 448, 42, 107, 675, 81, 483, 105, 409, 882, 977, 588, 321, 247, 648, 555, 613, 526, 124, 415, 84, 523, 102, 871, 389, 929, 418, 740, 879, 612, 294, 174, 143, 653, 228, 647, 697, 690, 271, 859, 512, 315, 870, 600, 817, 149, 13, 419, 527, 699, 491, 114, 704, 763, 713, 751, 283, 121, 994, 863, 298, 174, 693, 167, 439, 163, 139, 966, 69, 382, 999, 998, 596, 234, 913, 89, 527, 244, 774, 591, 349, 684, 978, 847, 465, 431, 151, 943, 655, 749, 278, 63, 244, 863, 91, 43, 63, 386, 897, 386, 888, 253, 108, 538, 572, 954, 781, 696, 517, 66, 990, 283, 399, 128, 351, 911, 821, 574, 787, 588, 785, 397, 971, 271, 222, 158, 147, 412, 580, 272, 747, 300, 684, 304, 992, 218, 108, 137, 699, 761, 323, 868, 279, 312, 998, 62, 387, 569, 132, 173, 786, 385, 924, 724, 341, 108, 645, 730, 53, 541, 441, 258, 584, 402, 856, 586, 889, 838, 341, 930, 591, 934, 431, 115, 128, 670, 31, 821, 384, 728, 656, 441, 25, 518, 374, 542, 660, 511, 921, 139, 651, 374, 383, 494, 881, 104, 132, 822, 930, 79, 584, 968, 293, 723, 851, 401, 445, 62, 557, 769, 923, 623, 958, 14, 877, 14, 584, 868, 618, 866, 368, 861, 296, 838, 776, 88, 782, 294, 898, 273, 115, 236, 412, 373, 171, 765, 731, 588, 839, 700, 932, 405, 112, 629, 998, 845, 961, 418, 882, 910, 256, 533, 574, 26, 809, 85, 376, 123, 477, 288, 336, 955, 234, 194, 956, 218, 347, 803, 84, 28, 856, 743, 535, 397, 507, 429, 376, 140, 521, 628, 608, 654, 472, 300, 295, 248, 693, 797, 656, 516, 61, 626, 25, 641, 374, 258, 373, 326, 150, 899, 35, 492, 825, 855, 75, 138, 126, 829, 881, 743, 869, 633, 988, 769, 719, 757, 778, 292, 65, 748, 252, 669, 533, 151, 963, 916, 21, 505, 951, 500, 389, 733, 729, 555, 671, 438, 393, 689, 805, 274, 445, 525, 986, 74, 316, 673, 874, 394, 463, 472, 172, 99, 354, 601, 865, 614, 681, 696, 752, 528, 425, 565, 28, 868, 385, 75, 43, 531, 579, 122, 586, 94, 109, 188, 407, 214, 974, 147, 660, 851, 234, 319, 408, 221, 705, 854, 102, 490, 409, 750, 887, 888, 313, 497, 639, 330, 664, 332, 613, 878, 463, 905, 42, 340, 326, 76, 118, 432, 649, 714, 975, 220, 648, 741, 875, 91, 98, 70, 591, 746, 12, 309, 180, 651, 161, 4, 334, 671, 677, 459, 592, 708, 172, 126, 103, 868, 102, 454, 789, 2, 725, 277, 143, 683, 518, 433, 400, 71, 727, 519, 292, 954, 649, 156, 872, 100, 905, 990, 897, 773, 691, 888, 505, 506, 960, 556, 459, 773, 851, 141, 807, 217, 781, 476, 359, 572, 686, 765, 295, 17, 46, 918, 308, 686, 849, 706, 614, 397, 813, 827, 941, 302, 527, 286, 854, 58, 447, 283, 965, 98, 747, 255, 876, 268, 209, 305, 686, 150, 878, 105, 763, 144, 645, 910, 619, 55, 413, 622, 135, 669, 924, 851, 73, 759, 157, 982, 653, 933, 907, 94, 78, 590, 647, 675, 53, 917, 71, 29, 294, 576, 523, 772, 457, 109, 953, 422, 3, 447, 400, 48, 427, 446, 968, 333, 372, 700, 567, 854, 182, 778, 105, 687, 502, 135, 502, 460, 560, 963, 78, 934, 162, 543, 199, 45, 945, 857, 828, 828, 974, 815, 168, 235, 767, 530, 774, 690, 742, 758, 539, 187, 964, 523, 448, 454, 973, 487, 518, 136, 527, 955, 168, 719, 575, 505, 571, 728, 844, 414, 34, 105, 863, 582, 872, 417, 457, 906, 538, 264, 632, 749, 685, 428, 894, 839, 104, 665, 814, 411, 475, 992, 121, 822, 510, 872, 91, 77, 72, 414, 112, 25, 801, 849, 90, 246, 58, 762, 68, 325, 116, 105, 714, 513, 208, 711, 190, 408, 10, 417, 763, 34, 641, 266, 443, 338, 922, 362, 333, 678, 838, 705, 106, 225, 594, 866, 723, 162, 366, 372, 35, 999, 690, 400, 780, 139, 470, 170, 995, 504, 751, 902, 81, 814, 288, 963, 913, 30, 606, 481, 461, 57, 105, 571, 259, 927, 540, 235, 357, 651, 178, 313, 814, 221, 499, 491, 392, 173, 831, 505, 221, 742, 526, 880, 619, 399, 202, 384, 550, 506, 565, 922, 632, 384, 11, 141, 62, 15, 906, 560, 63, 78, 961, 302, 692, 933, 855, 760, 25, 505, 955, 91, 821, 405, 754, 856, 241, 255, 207, 309, 618, 756, 598, 892, 657, 452, 447, 570, 773, 433, 929, 53, 715, 818, 807, 872, 729, 581, 573, 437, 211, 997, 754, 498, 774, 644, 647, 128, 979, 728, 444, 923, 846, 847, 774, 403, 726, 758, 479, 756, 889, 390, 870, 290, 440, 651, 466, 568, 845, 425, 264, 985, 51, 788, 486, 157, 803, 815, 206, 432, 811, 453, 469, 782, 339, 449, 423, 422, 18, 836, 626, 875, 587, 768, 923, 261, 948, 37, 878, 323, 312, 679, 80, 188, 717, 336, 936, 441, 239, 273, 994, 641, 697, 753, 499, 66, 989, 69, 434, 603, 61, 944, 535, 990, 292, 508, 747, 177, 935, 448, 964, 717, 997, 784, 166, 250, 848, 664, 278, 152, 514, 40, 265, 358, 709, 795, 494, 717, 302, 631, 915, 708, 48, 168, 426, 441, 129, 902, 151, 142, 413, 456, 237, 15, 410, 485, 554, 363, 339, 949, 537, 169, 134, 854, 156, 620, 545, 225, 377, 12, 718, 162, 925, 873, 764, 697, 853, 997, 605, 851, 710, 248, 124, 993, 319, 508, 350, 918, 85, 259, 754, 546, 748, 915, 989, 715, 817, 799, 297, 266, 707, 581, 369, 845, 363, 176, 571, 943, 67, 410, 915, 830, 89, 687, 38, 760, 133, 990, 366, 872, 534, 575, 516, 989, 958, 424, 10, 172, 414, 128, 170, 789, 895, 447, 608, 967, 927, 892, 990, 461, 50, 705, 153, 339, 481, 895, 563, 113, 952, 259, 63, 729, 408, 279, 768, 287, 671, 322, 353, 439, 619, 895, 791, 252, 444, 410, 343, 855, 695, 977, 242, 857, 643, 879, 512, 229, 518, 673, 809, 318, 614, 492, 213, 967, 490, 529, 102, 114, 550, 257, 991, 122, 275, 83, 682, 323, 494, 100, 644, 848, 555, 114, 926, 44, 278, 986, 553, 460, 323, 894, 371, 147, 720, 262, 248, 365, 550, 390, 438, 970, 414, 975, 785, 993, 642, 529, 720, 979, 92, 588, 70, 729, 96, 224, 887, 873, 270, 132, 462, 132, 611, 732, 898, 71, 166, 364, 47, 829, 361, 902, 593, 988, 711, 386, 534, 779, 979, 354, 503, 899, 203, 206, 524, 581, 696, 273, 410, 113, 423, 767, 682, 440, 475, 994, 465, 273, 32, 755, 362, 423, 410, 693, 710, 607, 40, 103, 161, 616, 640, 46, 179, 679, 995, 934, 855, 646, 263, 536, 955, 726, 833, 935, 445, 509, 769, 112, 101, 706, 803, 90, 538, 850, 470, 995, 989, 516, 321, 891, 19, 276, 982, 648, 820, 75, 934, 425, 389, 517, 959, 867, 835, 425, 990, 437, 345, 533, 219, 915, 595, 511, 485, 96, 302, 679, 882, 827, 120, 756, 452, 649, 73, 16, 978, 779, 964, 979, 172, 787, 521, 609, 344, 238, 814, 396, 274, 293, 392, 726, 778, 634, 138, 687, 33, 405, 861, 809, 275, 796, 305, 207, 785, 832, 67, 524, 839, 151, 598, 697, 455, 679, 453, 828, 753, 660, 492, 289, 432, 423, 446, 442, 216, 996, 540, 277, 468, 505, 49, 57, 65, 282, 941, 762, 466, 521, 896, 637, 817, 780, 356, 777, 642, 684, 791, 311, 850, 505, 102, 605, 385, 547, 533, 604, 770, 908, 888, 801, 688, 594, 839, 738, 133, 253, 225, 306, 892, 289, 198, 971, 337, 85, 912, 135, 208, 708, 107, 538, 9, 27, 857, 238, 269, 833, 206, 754, 786, 837, 573, 599, 879, 250, 229, 658, 495, 604, 545, 846, 84, 901, 371, 746, 819, 431, 738, 834, 762, 150, 255, 696, 353, 537, 557, 769, 477, 976, 757, 385, 822, 482, 495, 692, 867, 65, 882, 963, 552, 202, 452, 737, 351, 481, 547, 471, 684, 638, 345, 168, 675, 451, 384, 206, 560, 838, 151, 140, 965, 961, 923, 972, 55, 634, 532, 788, 784, 923, 970, 88, 111, 902, 308, 863, 99, 385, 899, 539, 104, 490, 406, 13, 368, 1, 352, 707, 260, 209, 698, 808, 546, 381, 615, 18, 486, 790, 910, 380, 812, 429, 173, 971, 72, 652, 905, 618, 727, 573, 365, 801, 149, 609, 237, 274, 92, 715, 208, 263, 749, 203, 66, 545, 286, 495, 689, 650, 537, 678, 970, 61, 373, 872, 646, 41, 310, 844, 428, 476, 745, 117, 709, 956, 725, 478, 177, 436, 749, 836, 877, 312, 110, 649, 803, 812, 904, 16, 673, 781, 665, 311, 602, 93, 938, 878, 989, 675, 953, 305, 762, 903, 149, 778, 656, 380, 750, 270, 707, 932, 603, 139, 49, 716, 334, 537, 951, 365, 288, 53, 904, 821, 551, 321, 434, 725, 230, 298, 502, 568, 859, 410, 411, 349, 164, 976, 778, 601, 48, 774, 27, 874, 958, 134, 872, 565, 831, 745, 181, 654, 270, 409, 104, 714, 297, 522, 759, 989, 842, 573, 849, 659, 955, 722, 885, 236, 679, 489, 470, 632, 250, 228, 89, 689, 880, 956, 100, 790, 852, 178, 818, 200, 870, 402, 784, 510, 571, 961, 873, 268, 570, 301, 360, 443, 386, 167, 193, 580, 20, 522, 66, 732, 953, 25, 530, 827, 817, 407, 962, 929, 390, 242, 31, 130, 567, 443, 393, 993, 216, 706, 913, 248, 736, 402, 664, 851, 268, 200, 555, 223, 874, 54, 106, 800, 656, 389, 484, 166, 206, 138, 777, 430, 351, 230, 903, 266, 183, 782, 154, 628, 153, 455, 780, 301, 41, 935, 449, 372, 671, 589, 323, 641, 684, 808, 564, 963, 452, 156, 376, 426, 535, 321, 296, 546, 381, 182, 410, 713, 365, 375, 632, 368, 127, 504, 767, 488, 463, 757, 874, 5, 599, 3, 788, 769, 583, 539, 282, 458, 718, 551, 735, 76, 378, 106, 807, 60, 828, 980, 382, 738, 322, 188, 822, 915, 806, 139, 212, 637, 933, 649, 733, 81, 134, 543, 634, 62, 188, 742, 424, 822, 577, 589, 879, 607, 685, 76, 216, 467, 853, 697, 523, 227, 250, 210, 888, 888, 568, 217, 939, 301, 374, 902, 39, 944, 549, 289, 14, 811, 376, 84, 460, 108, 900, 270, 319, 774, 20, 415, 815, 438, 758, 592, 544, 792, 577, 2, 888, 194, 487, 24, 565, 411, 784, 512, 794, 697, 860, 367, 609, 992, 864, 353, 348, 682, 422, 62, 223, 939, 55, 150, 612, 496, 610, 518, 292, 870, 319, 492, 280, 429, 851, 474, 542, 2, 356, 142, 184, 783, 437, 836, 870, 29, 821, 171, 241, 460, 227, 286, 269, 540, 369, 5, 456, 641, 824, 979, 959, 209, 967, 963, 554, 497, 396, 292, 561, 509, 83, 191, 40, 537, 246, 207, 555, 785, 356, 506, 199, 738, 511, 230, 242, 117, 722, 199, 881, 109, 5, 177, 65, 369, 203, 487, 919, 720, 454, 87, 225, 875, 280, 732, 191, 546, 952, 502, 227, 175, 185, 209, 678, 178, 901, 969, 607, 298, 231, 385, 406, 648, 939, 770, 494, 352, 249, 106, 487, 803, 836, 273, 710, 293, 490, 297, 106, 869, 848, 677, 918, 788, 961, 297, 316, 223, 125, 852, 252, 6, 913, 61, 159, 880, 2, 305, 782, 332, 6, 399, 116, 50, 436, 548, 547, 639, 646, 21, 996, 335, 56, 954, 510, 5, 520, 526, 906, 637, 314, 285, 881, 89, 343, 511, 92, 895, 867, 978, 249, 611, 158, 848, 443, 985, 447, 288, 631, 154, 836, 704, 935, 140, 141, 553, 244, 943, 909, 618, 272, 180, 921, 197, 770, 786, 271, 653, 80, 500, 486, 756, 149, 657, 411, 391, 693, 473, 906, 935, 932, 70, 689, 677, 395, 997, 594, 739, 492, 335, 463, 310, 963, 483, 577, 636, 240, 632, 470, 402, 649, 736, 937, 711, 387, 738, 197, 101, 783, 135, 539, 799, 992, 408, 875, 381, 769, 219, 655, 957, 59, 197, 96, 24, 407, 310, 497, 834, 258, 679, 518, 159, 342, 6, 948, 620, 622, 612, 998, 457, 286, 477, 902, 981, 427, 702, 434, 487, 105, 878, 48, 192, 181, 872, 261, 923, 492, 400, 634, 89, 501, 509, 11, 168, 407, 11, 291, 522, 644, 22, 948, 256, 713, 924, 109, 568, 655, 491, 756, 209, 987, 440, 810, 694, 668, 70, 911, 888, 411, 509, 169, 505, 524, 561, 125, 332, 13, 312, 679, 452, 816, 209, 21, 334, 517, 993, 605, 350, 486, 253, 32, 152, 120, 901, 682, 335, 329, 556, 248, 999, 842, 304, 241, 144, 84, 759, 35, 746, 746, 809, 793, 305, 672, 625, 618, 220, 275, 442, 237, 100, 142, 849, 764, 805, 303, 119, 791, 893, 320, 782, 82, 382, 478, 300, 210, 616, 652, 590, 90, 458, 446, 426, 693, 252, 687, 84, 622, 858, 302, 998, 876, 433, 157, 38, 390, 230, 691, 918, 554, 147, 635, 580, 835, 158, 934, 506, 50, 239, 523, 35, 858, 199, 947, 169, 873, 312, 525, 886, 175, 91, 863, 563, 815, 434, 509, 308, 821, 5, 911, 713, 442, 621, 447, 320, 343, 789, 357, 776, 211, 108, 261, 286, 396, 751, 482, 953, 22, 833, 469, 697, 616, 20, 858, 609, 743, 423, 219, 226, 264, 268, 417, 415, 69, 361, 337, 821, 377, 991, 679, 277, 738, 218, 580, 83, 419, 479, 177, 517, 324, 915, 39, 244, 840, 890, 683, 605, 277, 736, 625, 458, 816, 803, 237, 230, 601, 419, 40, 474, 133, 227, 119, 461, 16, 64, 289, 934, 326, 348, 219, 370, 25, 845, 408, 733, 126, 987, 753, 350, 401, 548, 486, 741, 334, 967, 304, 524, 442, 503, 902, 23, 710, 239, 882, 606, 734, 403, 476, 837, 577, 19, 211, 10, 659, 926, 639, 357, 613, 834, 428, 902, 880, 86, 514, 278, 806, 161, 416, 374, 238, 234, 430, 840, 940, 370, 41, 841, 443, 359, 346, 882, 586, 884, 565, 788, 965, 186, 350, 448, 299, 787, 938, 295, 530, 476, 706, 528, 582, 495, 606, 244, 634, 904, 386, 624, 433, 213, 117, 593, 921, 462, 299, 968, 405, 647, 381, 892, 352, 937, 342, 697, 397, 326, 91, 943, 831, 116, 613, 373, 732, 527, 117, 552, 546, 809, 256, 219, 585, 324, 1, 370, 946, 79, 857, 834, 770, 188, 306, 586, 296, 943, 612, 686, 497, 504, 997, 403, 510, 635, 870, 140, 22, 282, 62, 716, 467, 883, 356, 423, 569, 541, 737, 454, 794, 94, 442, 803, 397, 543, 212, 569, 190, 149, 886, 391, 384, 111, 156, 572, 440, 874, 480, 878, 924, 406, 641, 449, 811, 773, 798, 620, 501, 844, 766, 804, 359, 716, 838, 283, 600, 289, 910, 28, 232, 355, 170, 940, 182, 408, 781, 514, 336, 888, 505, 388, 343, 329, 230, 631, 141, 83, 425, 992, 965, 623, 898, 733, 753, 660, 487, 449, 324, 320, 611, 173, 630, 631, 83, 730, 651, 246, 295, 12, 551, 157, 461, 746, 649, 525, 689, 767, 521, 167, 926, 488, 365, 491, 373, 794, 270, 607, 432, 574, 342, 421, 656, 850, 206, 397, 944, 854, 503, 280, 115, 987, 208, 259, 999, 788, 198, 230, 391, 494, 148, 795, 599, 668, 471, 336, 930, 937, 509, 473, 114, 367, 679, 565, 132, 895, 796, 649, 122, 465, 318, 399, 64, 938, 960, 465, 989, 322, 201, 997, 63, 949, 359, 842, 211, 721, 508, 258, 372, 738, 817, 115, 293, 185, 148, 218, 240, 111, 854, 714, 37, 168, 178, 688, 618, 386, 266, 321, 33, 994, 38, 362, 755, 990, 326, 190, 619, 178, 987, 137, 914, 235, 722, 1000, 126, 168, 612, 675, 638, 392, 595, 958, 284, 691, 309, 857, 720, 268, 940, 573, 568, 873, 795, 709, 110, 458, 395, 429, 122, 948, 693, 248, 99, 457, 840, 555, 140, 190, 908, 738, 876, 138, 273, 831, 442, 594, 675, 486, 977, 223, 825, 885, 42, 396, 157, 998, 79, 86, 671, 930, 300, 623, 868, 639, 679, 516, 939, 237, 749, 243, 574, 622, 757, 507, 468, 654, 561, 781, 498, 621, 798, 800, 81, 755, 227, 867, 11, 88, 844, 714, 961, 813, 20, 663, 116, 893, 292, 458, 774, 146, 760, 792, 177, 728, 185, 235, 798, 744, 724, 90, 789, 419, 713, 512, 84, 969, 384, 736, 325, 9, 515, 336, 343, 191, 851, 564, 663, 181, 663, 239, 612, 573, 369, 436, 431, 281, 876, 775, 542, 195, 43, 160, 886, 386, 363, 562, 873, 768, 417, 82, 788, 138, 976, 403, 490, 459, 505, 515, 27, 640, 982, 33, 16, 15, 959, 34, 717, 662, 387, 44, 320, 431, 346, 725, 892, 467, 322, 168, 50, 657, 97, 460, 253, 329, 902, 541, 428, 782, 358, 376, 195, 260, 448, 618, 755, 20, 237, 813, 587, 431, 684, 313, 592, 672, 382, 226, 398, 798, 592, 545, 662, 452, 757, 3, 363, 410, 369, 197, 597, 735, 301, 717, 216, 977, 98, 171, 22, 424, 486, 709, 104, 669, 506, 990, 350, 720, 350, 482, 371, 956, 204, 385, 308, 12, 108, 453, 363, 120, 164, 968, 302, 443, 721, 640, 708, 288, 538, 122, 110, 674, 699, 99, 537, 823, 835, 924, 783, 30, 381, 505, 338, 807, 714, 426, 700, 529, 319, 681, 462, 539, 668, 323, 470, 771, 591, 674, 196, 455, 479, 251, 701, 378, 286, 98, 365, 621, 39, 930, 828, 196, 246, 19, 419, 622, 917, 845, 603, 817, 174, 997, 610, 211, 660, 498, 809, 3, 347, 537, 228, 849, 187, 750, 125, 313, 502, 886, 752, 129, 267, 975, 34, 89, 82, 145, 693, 526, 166, 304, 377, 581, 866, 537, 932, 431, 261, 380, 223, 730, 325, 504, 12, 5, 904, 138, 106, 871, 12, 630, 13, 711, 365, 776, 26, 421, 282, 297, 80, 599, 720, 780, 529, 484, 928, 828, 406, 413, 390, 370, 621, 452, 735, 149, 340, 267, 800, 682, 825, 74, 961, 98, 349, 158, 849, 536, 437, 42, 569, 776, 581, 139, 711, 1, 971, 554, 661, 950, 866, 714, 182, 589, 176, 659, 713, 362, 328, 437, 624, 224, 681, 998, 808, 38, 527, 312, 228, 45, 939, 976, 831, 299, 892, 355, 208, 947, 263, 797, 243, 575, 147, 826, 243, 853, 373, 371, 440, 32, 691, 314, 941, 999, 934, 960, 665, 249, 422, 132, 309, 752, 27, 196, 767, 225, 867, 697, 906, 628, 786, 789, 74, 19, 953, 297, 337, 90, 837, 765, 387, 612, 504, 442, 775, 849, 325, 249, 796, 833, 934, 982, 874, 410, 246, 200, 37, 395, 904, 165, 347, 975, 779, 371, 589, 896, 903, 545, 459, 988, 270, 36, 337, 459, 933, 60, 651, 583, 334, 939, 50, 199, 340, 507, 295, 968, 845, 640, 329, 176, 866, 28, 420, 956, 145, 847, 192, 884, 816, 749, 94, 554, 480, 964, 257, 661, 17, 613, 610, 758, 527, 903, 535, 127, 702, 568, 227, 18, 183, 833, 119, 634, 906, 797, 374, 362, 266, 786, 584, 44, 36, 397, 694, 365, 285, 859, 911, 548, 363, 414, 79, 843, 10, 8, 688, 626, 561, 694, 230, 993, 492, 357, 332, 588, 146, 899, 468, 375, 897, 471, 564, 184, 472, 741, 877, 67, 689, 77, 244, 411, 491, 167, 790, 441, 134, 722, 997, 907, 223, 250, 914, 241, 161, 454, 859, 727, 413, 254, 818, 288, 997, 95, 348, 42, 751, 850, 13, 331, 9, 52, 532, 907, 774, 549, 852, 534, 218, 748, 106, 825, 273, 137, 572, 138, 415, 261, 386, 730, 910, 860, 955, 670, 754, 512, 775, 983, 662, 64, 244, 124, 999, 661, 210, 884, 110, 45, 737, 869, 524, 743, 232, 817, 49, 954, 964, 153, 704, 334, 690, 178, 994, 279, 525, 968, 319, 573, 954, 98, 925, 684, 179, 763, 322, 113, 610, 402, 575, 899, 37, 189, 738, 99, 393, 232, 164, 838, 275, 666, 185, 385, 335, 334, 599, 616, 452, 733, 744, 248, 70, 459, 617, 923, 681, 254, 193, 866, 778, 656, 453, 790, 506, 255, 236, 389, 848, 58, 988, 875, 349, 554, 302, 727, 96, 615, 215, 695, 225, 148, 349, 719, 865, 54, 570, 165, 882, 400, 377, 614, 75, 472, 955, 838, 85, 740, 220, 135, 498, 263, 183, 479, 308, 894, 721, 885, 171, 485, 917, 543, 695, 227, 53, 581, 285, 647, 905, 244, 725, 320, 769, 774, 44, 271, 271, 486, 139, 915, 987, 648, 750, 351, 240, 888, 112, 682, 250, 543, 716, 156, 924, 260, 464, 311, 697, 676, 52, 439, 695, 956, 541, 730, 127, 652, 470, 278, 69, 846, 43, 758, 264, 378, 859, 744, 408, 688, 99, 61, 82, 163, 315, 141, 519, 400, 98, 117, 871, 771, 322, 395, 380, 694, 633, 338, 287, 126, 279, 954, 209, 166, 28, 410, 542, 485, 345, 332, 211, 462, 709, 820, 709, 305, 821, 510, 214, 792, 174, 380, 528, 376, 832, 7, 578, 113, 696, 239, 401, 625, 400, 503, 262, 610, 205, 308, 791, 265, 569, 148, 653, 351, 253, 8, 425, 335, 804, 554, 901, 594, 53, 672, 603, 297, 11, 177, 914, 666, 481, 123, 11, 401, 809, 754, 72, 368, 418, 298, 182, 223, 788, 944, 28, 121, 722, 24, 433, 227, 330, 841, 92, 1000, 186, 457, 592, 162, 939, 546, 711, 170, 908, 240, 824, 279, 616, 932, 528, 288, 158, 904, 670, 516, 660, 135, 457, 589, 25, 835, 914, 176, 58, 971, 868, 263, 235, 497, 179, 451, 996, 844, 956, 501, 734, 297, 486, 411, 194, 177, 892, 101, 266, 911, 281, 841, 375, 309, 982, 468, 132, 185, 80, 79, 228, 182, 988, 200, 109, 967, 233, 128, 71, 752, 377, 612, 813, 382, 496, 494, 240, 157, 515, 396, 950, 316, 152, 855, 396, 362, 178, 650, 280, 148, 340, 40, 79, 186, 556, 787, 520, 814, 583, 674, 541, 727, 744, 231, 663, 780, 134, 417, 477, 79, 670, 469, 320, 102, 830, 376, 251, 922, 352, 766, 757, 757, 442, 138, 496, 781, 943, 558, 948, 702, 687, 525, 655, 87, 516, 952, 837, 128, 343, 366, 38, 905, 768, 208, 181, 959, 397, 714, 229, 498, 132, 535, 555, 503, 166, 908, 550, 219, 453, 559, 675, 497, 782, 628, 906, 783, 871, 170, 615, 207, 291, 696, 979, 899, 223, 604, 754, 616, 223, 567, 152, 917, 695, 721, 869, 646, 752, 495, 93, 578, 992, 778, 365, 241, 171, 220, 516, 707, 726, 289, 711, 265, 366, 874, 524, 449, 123, 832, 829, 551, 170, 22, 579, 30, 585, 685, 123, 904, 465, 431, 653, 176, 895, 828, 750, 628, 267, 452, 119, 603, 136, 842, 566, 704, 778, 438, 852, 441, 946, 970, 92, 156, 272, 319, 466, 283, 643, 177, 134, 62, 930, 625, 444, 503, 788, 481, 938, 217, 634, 576, 180, 231, 144, 728, 945, 284, 939, 237, 411, 288, 234, 422, 714, 305, 42, 380, 522, 358, 108, 70, 289, 57, 599, 281, 500, 640, 117, 692, 631, 344, 245, 143, 652, 776, 359, 143, 628, 309, 588, 88, 638, 75, 824, 431, 651, 110, 34, 866, 482, 53, 569, 434, 694, 321, 706, 389, 339, 511, 5, 63, 638, 469, 161, 988, 244, 510, 870, 82, 572, 61, 50, 264, 643, 406, 540, 454, 876, 63, 508, 918, 312, 248, 162, 838, 130, 763, 368, 789, 225, 274, 329, 723, 411, 474, 715, 55, 527, 788, 991, 2, 534, 813, 989, 628, 780, 827, 840, 18, 170, 199, 912, 930, 805, 369, 408, 927, 564, 741, 9, 937, 35, 677, 439, 565, 971, 418, 956, 192, 815, 649, 44, 695, 80, 698, 987, 82, 327, 15, 825, 365, 359, 706, 349, 587, 652, 898, 522, 597, 692, 724, 805, 325, 309, 170, 414, 167, 212, 892, 146, 445, 27, 625, 265, 486, 945, 665, 717, 17, 631, 956, 976, 173, 797, 740, 69, 733, 137, 1000, 370, 81, 618, 798, 475, 370, 715, 165, 923, 635, 649, 819, 133, 610, 879, 515, 398, 380, 182, 442, 486, 790, 841, 458, 101, 698, 113, 43, 807, 752, 2, 420, 18, 579, 233, 315, 410, 82, 834, 556, 698, 488, 65, 168, 916, 543, 386, 131, 16, 370, 569, 969, 544, 751, 747, 632, 654, 604, 107, 87, 306, 920, 167, 226, 822, 161, 966, 339, 187, 408, 465, 838, 786, 466, 639, 412, 626, 865, 908, 401, 810, 180, 691, 341, 427, 134, 778, 429, 501, 505, 288, 74, 893, 819, 629, 85, 920, 29, 216, 440, 581, 703, 267, 123, 834, 248, 66, 635, 214, 306, 990, 716, 820, 703, 545, 225, 516, 466, 588, 813, 670, 295, 999, 839, 16, 524, 35, 77, 582, 408, 685, 525, 101, 253, 991, 398, 37, 788, 685, 690, 871, 143, 379, 183, 374, 593, 738, 788, 786, 902, 70, 853, 958, 83, 214, 652, 821, 697, 725, 715, 583, 861, 509, 637, 689, 745, 847, 861, 163, 616, 559, 415, 40, 494, 648, 829, 883, 622, 377, 862, 102, 777, 149, 819, 197, 866, 984, 291, 842, 442, 808, 842, 304, 181, 689, 700, 264, 190, 862, 208, 960, 704, 640, 867, 350, 936, 280, 128, 249, 871, 191, 929, 824, 417, 255, 345, 614, 877, 938, 803, 305, 384, 633, 751, 775, 119, 635, 757, 288, 696, 967, 848, 353, 949, 184, 134, 109, 753, 638, 933, 700, 95, 156, 485, 105, 844, 213, 113, 275, 893, 798, 699, 795, 872, 847, 58, 970, 164, 705, 508, 157, 537, 462, 860, 522, 449, 443, 63, 991, 700, 44, 950, 16, 753, 474, 44, 174, 892, 963, 190, 783, 860, 561, 620, 770, 958, 864, 402, 661, 244, 573, 128, 785, 644, 324, 192, 577, 383, 878, 683, 977, 348, 652, 326, 936, 456, 912, 994, 468, 582, 971, 66, 204, 971, 758, 210, 935, 896, 278, 427, 664, 792, 905, 528, 306, 39, 965, 434, 956, 444, 144, 989, 175, 92, 374, 288, 708, 100, 827, 776, 732, 938, 751, 787, 454, 316, 378, 831, 327, 544, 649, 910, 298, 861, 814, 119, 274, 709, 272, 583, 469, 677, 118, 950, 794, 267, 734, 664, 227, 813, 286, 612, 582, 312, 980, 476, 488, 509, 298, 776, 112, 741, 575, 116, 704, 109, 380, 400, 768, 772, 331, 554, 405, 655, 447, 790, 672, 771, 339, 199, 622, 238, 581, 972, 874, 302, 964, 670, 98, 656, 889, 797, 729, 414, 531, 25, 908, 432, 903, 341, 961, 331, 873, 697, 945, 12, 96, 432, 735, 117, 787, 444, 935, 397, 711, 868, 497, 628, 992, 592, 708, 983, 367, 62, 842, 820, 913, 650, 217, 708, 746, 781, 811, 866, 421, 834, 268, 114, 740, 488, 553, 469, 536, 338, 888, 234, 693, 454, 702, 810, 445, 210, 564, 688, 169, 813, 910, 868, 662, 149, 806, 256, 582, 959, 228, 601, 742, 184, 490, 632, 550, 396, 715, 185, 109, 620, 912, 360, 177, 279, 52, 451, 648, 169, 328, 815, 961, 996, 802, 983, 697, 309, 334, 805, 156, 395, 992, 845, 390, 358, 647, 445, 865, 418, 624, 330, 486, 862, 935, 500, 836, 337, 950, 296, 847, 995, 732, 55, 915, 920, 269, 62, 913, 464, 621, 324, 164, 621, 494, 801, 546, 401, 561, 864, 54, 225, 748, 894, 798, 384, 218, 919, 650, 942, 72, 371, 556, 937, 695, 114, 814, 201, 742, 323, 992, 315, 329, 114, 379, 826, 418), 2797))

}
