import sys
import matplotlib.pyplot as plt
import numpy as np


def generate_graph(path, case_name):
    dataset = {
        'build_num': [],
        'master_version': [],
        'master_xa': [],
        '4.1.1_version': [],
        '3.0.0_version': [],
        'mysql_server': []
    }
    with open(path + '/.build_number.txt') as builds:
        for line in builds:
            dataset['build_num'].append(int(line))
    generate_data(path, case_name, dataset)
    print(dataset)
    fig, ax = plt.subplots()
    ax.grid(True)
    plt.title(case_name)

    data = [dataset['master_version'][-7:], dataset['master_xa'][-7:], dataset['4.1.1_version'][-7:], dataset['3.0.0_version'][-7:], dataset['mysql_server'][-7:]]
    columns = dataset['build_num'][-7:]
    rows = ['master', 'xa', '4.1.1', '3.0.0', 'mysql']
    rcolors = plt.cm.BuPu(np.full(len(rows), 0.1))
    ccolors = plt.cm.BuPu(np.full(len(columns), 0.1))
    the_table = plt.table(cellText=data, rowLabels=rows, colLabels=columns, rowColours=rcolors, colColours=ccolors,
                          loc='bottom', bbox=[0.0, -0.50, 1, .28])
    plt.subplots_adjust(left=0.15, bottom=0.3, right=0.98)

    plt.xticks(range(14))
    ax.set_xticklabels(dataset['build_num'])
    plt.plot(dataset['master_version'], 'o-', color='magenta', label='master_version')
    plt.plot(dataset['master_xa'], 'o-', color='darkviolet', label='master_xa')
    plt.plot(dataset['4.1.1_version'], 'r--', color='blue', label='4.1.1_version')
    plt.plot(dataset['3.0.0_version'], 'r--', color='orange', label='3.0.0_version')
    plt.plot(dataset['mysql_server'], 'r--', color='lime', label='mysql_server')
    plt.xlim()
    plt.legend()
    plt.xlabel('build_num')
    plt.ylabel('transactions per second')
    plt.savefig('graph/' + path + '/' + case_name)
    plt.show()


def generate_data(path, case_name, dataset):
    for build in dataset['build_num']:
        fill_dataset(build, case_name, dataset, path, 'master_version', '.master.txt')
        fill_dataset(build, case_name, dataset, path, 'master_xa', '.xa.txt')
        fill_dataset(build, case_name, dataset, path, '4.1.1_version', '.4_1_1.txt')
        fill_dataset(build, case_name, dataset, path, '3.0.0_version', '.3_0_0.txt')
        fill_dataset(build, case_name, dataset, path, 'mysql_server', '.mysql.txt')


def fill_dataset(build, case_name, dataset, path, version, suffix):
    try:
        with open(path + '/' + str(build) + '/' + case_name + suffix) as version_master:
            value = 0
            for line in version_master:
                if 'transactions:' in line:
                    items = line.split('(')
                    value = float(items[1][:-10])
            dataset[version].append(value)
    except FileNotFoundError:
        dataset[version].append(0)


if __name__ == '__main__':
    path = sys.argv[1]
    generate_graph(path, 'oltp_point_select')
    generate_graph(path, 'oltp_read_only')
    generate_graph(path, 'oltp_write_only')
    generate_graph(path, 'oltp_readwrite')
    generate_graph(path, 'oltp_update_index')
    generate_graph(path, 'oltp_update_non_index')
    generate_graph(path, 'oltp_delete')

