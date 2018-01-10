import rospy

from threading import Event

from copy import deepcopy

from nav_msgs.msg import Odometry
from sensor_msgs.msg import PointCloud
from std_msgs.msg import Float64


class SubscriberCollector:
    def __init__(self, topics_dict):
        """
        topics_dict is of format: { 'scan': LaserScan, 'odom': Odometry, topic_name: datatype... }
        """
        self.topics = topics_dict

        self.events_in = {}

        self.data = {}

        self.events_out = {}

        for key in self.topics.keys():
            self.events_in[key] = Event()

            self.events_out[key] = Event()

            self.data[key] = None

    def create_subscriptions(self):
        for topic_name in self.topics.keys():
            data_type = self.topics[topic_name]

            rospy.Subscriber(topic_name, data_type, self.make_handler_func(topic_name))

            # print ("[Main] Created a new Subscriber for topic : " + topic_name + " with data type : " + str(data_type))

    def make_handler_func(self, tn):
        collector = self

        def handler(data_point):
            topic_name = tn

            # Wait to be authorised
            # print ("Thread for topic: " + topic_name + " is waiting...")

            collector.events_in[topic_name].wait()

            collector.events_in[topic_name].clear()

            # print ("Storing data from topic: " + topic_name)

            collector.data[topic_name] = data_point

            # print ("Notifying via Event for topic: " + topic_name)

            # Notify ready for this topic
            collector.events_out[topic_name].set()

        return handler

    def make_request(self):
        # Request new data
        print ("[Main] Requesting data in collected form")

        for key in self.topics.keys():
            # print ("[Main] Setting event in for topic: " + key)
            self.events_in[key].set()

    def wait_for_data(self):
        print ("[Main] Waiting for data to be assembled")

        # Wait for data
        for topic_name in self.topics.keys():
            # print ("[Main] Waiting for event signal for topic: " + topic_name)
            self.events_out[topic_name].wait()

            # print ("[Main] Clearing event signal for topic: " + topic_name)
            self.events_out[topic_name].clear()

        local_data = deepcopy(self.data)

        # print ("[Main] Returning data")

        return local_data


def ros_subscribers_collector_test():
    topics_dict = {'RosAria/odom': Odometry,
                   'RosAria/sonar': PointCloud,
                   '/RosAria/battery_voltage': Float64}

    rospy.init_node("test_thread_collector_node")

    collector = SubscriberCollector(topics_dict)

    collector.create_subscriptions()

    collector.make_request()

    index = 0

    while index <= 5:
        data = collector.wait_for_data()

        print ("[Main] Index: " + str(index) + ". I will sleep for 5 seconds to simulate slow processing")

        rospy.sleep(5)

        print ("[Main] I have finished sleeping")

        print ("[Main] Data (from 5 secs ago) was: ")

        print (data)

        print ("[Main] I will now make a new request for data.")
        collector.make_request()

        index += 1


if __name__ == '__main__':
    ros_subscribers_collector_test()
