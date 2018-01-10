import cv2
import numpy as np
import time

def make_movement_mask(magnitude, magnitude_threshold = 1):
    mask = magnitude.copy()

    mask[mask > magnitude_threshold] = 255

    mask = np.where(mask == 255, 255, 0)

    mask = mask.astype(np.uint8)

    return mask


def main():
    prevframe = None

    camera = cv2.VideoCapture(0)

    ok = True

    while ok:
        _, frame = camera.read()

        if (prevframe != None):
            start = time.time()

            pfg = cv2.cvtColor(prevframe, cv2.COLOR_BGR2GRAY)
            cfg = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)

            flow = cv2.calcOpticalFlowFarneback(pfg, cfg, None, 0.5, 3, 15, 3, 5, 1.2, 0)

            mag, angle = cv2.cartToPolar(flow[...,0], flow[...,1])

            movement_mask = make_movement_mask(mag, 6.75)

            segmented = cv2.bitwise_and(frame, frame, mask = movement_mask)

            _, cnt, hier = cv2.findContours(movement_mask.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)

            frame_out = frame.copy()

            for c in cnt:
                rect_cnt = cv2.boxPoints(cv2.minAreaRect(c)).astype(np.int32)

                cv2.drawContours(frame_out, [rect_cnt], 0, (0,0,255), 2)

            end = time.time()

            print ("Calculating movement mask took {} seconds".format(end - start))

            cv2.imshow("Movement mask", movement_mask)
            cv2.imshow("Moving objects", frame_out)
            cv2.imshow("Segmented", segmented)

        prevframe = frame.copy()

        k = cv2.waitKey(1) & 0xFF

        if k == 27:
            break

main()
