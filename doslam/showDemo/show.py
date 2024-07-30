import cv2  #OpenCV包
import numpy as np
import struct

def load_keypoints(data_file_path):
    keypoints = []
    with open(data_file_path, 'rb') as f:
        data = f.read()

    feature_size = 64  # 每个特征点的字节数
    num_features = len(data) // feature_size

    for i in range(num_features):
        offset = i * feature_size
        score = struct.unpack('i', data[offset:offset+4])[0]
        x = struct.unpack('i', data[offset+4:offset+8])[0]
        y = struct.unpack('i', data[offset+8:offset+12])[0]
        #size = 7.0  # 特征点的大小，可以根据需要调整
        kp = cv2.KeyPoint(x=float(x), y=float(y), size=7.0, response=float(score))
        keypoints.append(kp)

    print("kp length:", len(keypoints))
    return keypoints

# 首先确定原图片的基本信息：数据格式，行数列数，通道数
# rows=384#图像的行数
# cols=512#图像的列数
# channels =1# 图像的通道数，灰度图为1

rows1=480#图像的行数
cols1=640#图像的列数
channels1 =1# 图像的通道数，灰度图为1

rows2=384#图像的行数
cols2=512#图像的列数
channels2 =1# 图像的通道数，灰度图为1

rows3=307#图像的行数
cols3=416#图像的列数
channels3 =1# 图像的通道数，灰度图为1

rows4=245#图像的行数
cols4=328#图像的列数
channels4 =1# 图像的通道数，灰度图为1

# 利用numpy的fromfile函数读取raw文件，并指定数据格式
img1=np.fromfile(r'./showRaw/1_r480_c640.raw', dtype='uint8')
# 利用numpy中array的reshape函数将读取到的数据进行重新排列。
img1=img1.reshape(rows1, cols1, channels1)

img2=np.fromfile(r'./showRaw/1_r384_c512.raw', dtype='uint8')
# 利用numpy中array的reshape函数将读取到的数据进行重新排列。
img2=img2.reshape(rows2, cols2, channels2)

img3=np.fromfile(r'./showRaw/1_r307_c416.raw', dtype='uint8')
# 利用numpy中array的reshape函数将读取到的数据进行重新排列。
img3=img3.reshape(rows3, cols3, channels3)

img4=np.fromfile(r'./showRaw/1_r245_c328.raw', dtype='uint8')
# 利用numpy中array的reshape函数将读取到的数据进行重新排列。
img4=img4.reshape(rows4, cols4, channels4)


# draw_keypoints(img1, "./showRaw/1_r480_c640.raw.data")
kp1 = load_keypoints("./showRaw/1_r480_c640.raw.data")
img_with_keypoints1 = cv2.drawKeypoints(img1, kp1, img1)

kp2 = load_keypoints("./showRaw/1_r384_c512.raw.data")
img_with_keypoints2 = cv2.drawKeypoints(img2, kp2, img2)

kp3 = load_keypoints("./showRaw/1_r307_c416.raw.data")
img_with_keypoints3 = cv2.drawKeypoints(img3, kp3, img3)

kp4 = load_keypoints("./showRaw/1_r245_c328.raw.data")
img_with_keypoints4 = cv2.drawKeypoints(img4, kp4, img4)
# 展示图像
	# drawKeypoints(matSrc, fast_reference, disimg);
cv2.imshow('Infared image-r480_c640-8bit',img_with_keypoints1)
cv2.imshow('Infared image-r307_c416-8bit',img_with_keypoints2)
cv2.imshow('Infared image-r245_c328-8bit',img_with_keypoints3)
cv2.imshow('Infared image-r196_c264-8bit',img_with_keypoints4)
# 如果是uint16的数据请先转成uint8。不然的话，显示会出现问题。
cv2.waitKey()
cv2.destroyAllWindows()
print('ok')

